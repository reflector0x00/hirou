#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <stdexcept>
#include <regex>
#include <list>
#include <unordered_map>

//TODO: use constants
//TODO: divide files

class SyntaxError : public std::exception {
    std::string _msg;
    size_t _linePos;
    std::string _fullmsg;
public:
    SyntaxError(const std::string& msg, size_t linePos) : _msg(msg), _linePos(linePos), _fullmsg(_msg + " at line " + std::to_string(_linePos)) {}

    const char* what() const noexcept override {
        return _fullmsg.c_str();
    }
};

unsigned int htonl(unsigned int x) {
    unsigned char* s = (unsigned char*)&x;
    return (unsigned int)(s[0] << 24 | s[1] << 16 | s[2] << 8 | s[3]);
}

class AsmParser {
    size_t _linePos;
    size_t _addrPos;
    std::unordered_map<std::string, size_t> _labels;
    struct LabelNode {
        std::string label;
        size_t addrPos;
        size_t linePos;
        LabelNode(const std::string& Label, size_t AddrPos, size_t LinePos) : label(Label), addrPos(AddrPos), linePos(LinePos) {}
    };
    std::list<LabelNode> _labelList;

    size_t parseAlu(const std::string& alu) {
        if (alu == "add")
            return 0b0000;
        if (alu == "sub")
            return 0b0001;
        if (alu == "rsub")
            return 0b0010;
        if (alu == "or")
            return 0b0011;
        if (alu == "and")
            return 0b0100;
        if (alu == "rand")
            return 0b0101;
        if (alu == "xor")
            return 0b0110;
        if (alu == "xnor")
            return 0b0111;
        if (alu == "addc")
            return 0b1000;
        if (alu == "subc")
            return 0b1001;
        if (alu == "rsubc")
            return 0b1010;
        throw SyntaxError("Unknown alu microcommand \"" + alu + "\"", _linePos);
    }
    size_t parseRegister(const std::string& reg) {
        size_t ind;
        try {
            ind = (size_t)std::stoi(reg.substr(1));
        }
        catch (std::invalid_argument& e) {
            throw SyntaxError("Invalid operand \"" + reg + "\"", _linePos);
        }
        if (ind > 15)
            throw SyntaxError("Invalid operand \"" + reg + "\"", _linePos);
        return ind;
    }
    size_t parseInt(const std::string& val) {
        int ind;
        try {
            if (val[0] == '0') {
                if (val[1] == 'x')
                    ind = (size_t)std::stoi(val, nullptr, 16);
                else if (val[1] == 'b')
                    ind = (size_t)std::stoi(val.substr(2), nullptr, 2);
                else
                    ind = (size_t)std::stoi(val);
            }
            else
               ind = (size_t)std::stoi(val);
        }
        catch (std::invalid_argument& e) {
            throw SyntaxError("Invalid operand \"" + val + "\"", _linePos);
        }
        if ((ind < -16) || (ind > 15))
            throw SyntaxError("Value can't be greater than 15, but got \"" + val + "\"", _linePos);

        return (size_t)ind & 0b1111;
    }

    size_t parseOperand(const std::string& op, bool& isReg) {
        size_t ind;
        isReg = true;
        if (op == "RQ")
            ind = 16;
        else if (op[0] == 'R') {
            ind = parseRegister(op);
        }
        else {
            isReg = false;
            ind = parseInt(op);
        }
        return ind;
    }
    std::string checkComma(const std::string& str, bool comma) {
        if (comma) {
            if(str.back() != ',')
                throw SyntaxError("Expected comma after \"" + str + "\"", _linePos);
            return str.substr(0, str.length() - 1);
        }
        else if(str.back() == ',')
            throw SyntaxError("Unxpected comma after \"" + str + "\"", _linePos);
        return str;
    }
    size_t parseSourceRecieve(const std::string& r, const std::string& s, size_t& A, size_t& B, size_t& D) {
        bool isRegR;
        bool isRegS;
        size_t indR = parseOperand(r, isRegR);
        size_t indS = parseOperand(s, isRegS);
        size_t opc = 0;
        A = 0;
        B = 0;
        D = 0;

        if (isRegR) {
            if (isRegS) {
                A = indR;
                if (indS == 16)
                    opc = 0b000;
                else {
                    B = indS;
                    opc = 0b001;
                }
            }
            else
                throw SyntaxError("Wrong mode of S = " + s + ", R = " + r, _linePos);
        }
        else {
            D = indR;
            if (isRegS) {
                if (indS == 16) {
                    opc = indR == 0 ? 0b010 : 0b110;
                }
                else {
                    A = indS;
                    opc = indR == 0 ? 0b100 : 0b101; //RON(B)
                }
            }
            else if (indS == 0)
                opc = 0b111;
            else
                throw SyntaxError("Wrong mode of S = " + s + ", R = " + r, _linePos);
        }
        return opc;
    }
    size_t parseDestination(const std::string& op, size_t& A, size_t& B, size_t& source) {
        if (op == "0")
            return 0b0001;
        if (op == "RQ")
            return 0b0000;
        else if (op[0] == 'R') {
            size_t newB = parseRegister(op);
            if ((source == 0b001) && (B != newB))
                throw SyntaxError("Wrong mode of different Source and Dest", _linePos);
            B = newB;
            if ((source == 0b100) && (A == B)) {
                A = 0;
                --source;
            }
            return 0b0011;
        }
        else {
            std::string sub;
            bool modeL;
            if (op[0] == 'q') {
                modeL = false;
                sub = op.substr(1);
            }
            else {
                modeL = true;
                sub = op;
            }

            bool m0;
            bool m1;
            if ((sub[0] == 's') && (sub[1] == 'h')) {
                m1 = false;
                m0 = false;
            }
            else if ((sub[0] == 'r') && (sub[1] == 'o')) {
                m1 = false;
                m0 = true;
            }
            else if ((sub[0] == 'd') && (sub[1] == 'r')) {
                m1 = true;
                m0 = false;
            }
            else if ((sub[0] == 'a') && (sub[1] == 's')) {
                m1 = true;
                m0 = true;
            }
            else
                throw SyntaxError("Invalid operand \"" + op + "\"", _linePos);

            bool modeH;
            if (sub[2] == 'r')
                modeH = false;
            else if (sub[2] == 'l')
                modeH = true;
            else
                throw SyntaxError("Invalid operand \"" + op + "\"", _linePos);

            size_t newB = parseRegister(sub.substr(3));
            if ((source == 0b001) && (B != newB))
                throw SyntaxError("Wrong mode of different Source and Dest", _linePos);
            B = newB;

            if ((source == 0b100) && (A == B)) {
                A = 0;
                --source;
            }

            source |= m0 ? 0b1000 : 0;
            return (size_t)((m1 ? 0b1000 : 0) | 0b100 | (modeH ? 0b10 : 0) | (modeL ? 1 : 0));
        }
    }

    size_t parseJump(const std::string& jump, bool& ar) {
        size_t opcJump;
        if (jump == "jnz")
            opcJump = 0b0000;
        else if (jump == "jmp")
            opcJump = 0b0001;
        else if (jump == "jnxt")
            opcJump = 0b0010;
        else if (jump == "jadr")
            opcJump = 0b0011;
        else if (jump == "clnz")
            opcJump = 0b0100;
        else if (jump == "call")
            opcJump = 0b0101;
        else if (jump == "ret")
            opcJump = 0b0110;
        else if (jump == "jsp")
            opcJump = 0b0111;
        else if (jump == "jsnz")
            opcJump = 0b1000;
        else if (jump == "push")
            opcJump = 0b1001;
        else if (jump == "pop")
            opcJump = 0b1010;
        else if (jump == "jsnc4")
            opcJump = 0b1011;
        else if (jump == "jz")
            opcJump = 0b1100;
        else if (jump == "jf3")
            opcJump = 0b1101;
        else if (jump == "jovr")
            opcJump = 0b1110;
        else if (jump == "jc4")
            opcJump = 0b1111;
        else
            throw SyntaxError("Unknown jump condition \"" + jump + "\"", _linePos);

        switch (opcJump) {

            case 0b0010: //jnxt
            case 0b0110: //ret
            case 0b0111: //jsp
            case 0b1000: //jsnz
            case 0b1001: //push
            case 0b1010: //pop
            case 0b1011: //jsnc4
                ar = false;
                break;

            case 0b0011: //jaddr
                std::cerr << "Warning: jaddr isn't implemented well" << std::endl;
            case 0b0000: //jnz
            case 0b0001: //jmp
            case 0b0100: //clnz
            case 0b0101: //call
            case 0b1100: //jz
            case 0b1101: //jf3
            case 0b1110: //jovr
            case 0b1111: //jc4
                ar = true;
        }
        return opcJump;
    }
    size_t parseAddress(const std::string& a) {
        size_t addr = 0;
        try {
            addr = parseInt(a);
        }
        catch (SyntaxError &e) {
            if (checkLabel(a))
                _labelList.emplace_back(a, _addrPos, _linePos);
            else
                throw SyntaxError(e);
        }
        return addr;
    }
	char concat(size_t h, size_t l) {
		return (char)((h << 4) | l);
	}
	
    void writeRom(std::fstream& file, size_t opcAlu, size_t opcSource, size_t opcDestination, size_t A, size_t B, size_t D, size_t opcJump, size_t addr) {
		size_t a0 = addr & 0xF;
		size_t a1 = (addr >> 4) & 0xF;
		size_t a2 = (addr >> 8) & 0xF;
		
		file.put((char)0); // cmd
		file.put(concat(a2, a1)); 
		file.put(concat(a0, opcJump)); 
		file.put(concat(opcDestination, opcSource)); 
		file.put(concat(opcAlu, A)); 
		file.put(concat(B, D)); 
		
        
		// file.put((char)D);
        // file.put((char)B);
        // file.put((char)A);
        // file.put((char)opcAlu);
        // file.put((char)opcSource);
        // file.put((char)opcDestination);
        // file.put((char)opcJump);
        // file.put((char)addr);
    }
    std::string toBinary(size_t num) {
        std::string result(4, '0');
        result[0] = num & 0b1000 ? '1' : '0';
        result[1] = num & 0b0100 ? '1' : '0';
        result[2] = num & 0b0010 ? '1' : '0';
        result[3] = num & 0b0001 ? '1' : '0';
        return result;
    }
    void log(std::ostream& out, size_t opcAlu, size_t opcSource, size_t opcDestination, size_t A, size_t B, size_t D, size_t opcJump, size_t addr) {
        out << toBinary(addr) << " "
            << toBinary(opcJump) << " "
            << toBinary(opcDestination) << " "
            << toBinary(opcSource) << " "
            << toBinary(opcAlu) << " "
            << toBinary(A) << " "
            << toBinary(B) << " "
            << toBinary(D) << " "
            << std::endl;
    }
    void getArg(std::istream& in, std::string& arg, bool comma) {
        in >> arg;
        arg = checkComma(arg, comma);
    }
    inline void writeNop(std::fstream& file) {
        writeRom(file, 0b0100, 0b0111, 0b0001, 0b0000, 0b0000, 0b0000, 0b0010, 0b0000);
    }
    //TODO: unite it
    bool isLabel(const std::string& label) {
        static std::regex regexp("[a-zA-Z_]+[a-zA-Z0-9_]*\\:");
        return std::regex_match(label, regexp);
    }
    bool checkLabel(const std::string& label) {
        static std::regex regexp("[a-zA-Z_]+[a-zA-Z0-9_]*");
        return std::regex_match(label, regexp);
    }
    void setNop(size_t& opcAlu, size_t& opcSource, size_t& opcDestination, size_t& A, size_t& B, size_t& D) {
        opcAlu = 0b0100;
        opcSource = 0b0111;
        opcDestination = 0b0001;
        A = 0b0000;
        B = 0b0000;
        D = 0b0000;
    }
public:
    AsmParser() {}
    int parse(const std::string& inFName, const std::string& outFName) {
        std::ifstream inFile(inFName);
        if (!inFile.is_open()) {
            std::cerr << "Can't open file \"" << inFName << "\"" << std::endl;
            return 1;
        }
		std::ofstream cfile(outFName);
		if (!cfile.is_open()) {
            std::cerr << "Can't open file \"" << outFName << "\"" << std::endl;
            return 3;
        }	
		
        std::fstream outFile(outFName, std::ios::binary | std::ios::in | std::ios::out);
        if (!outFile.is_open()) {
            std::cerr << "Can't reopen file \"" << outFName << "\"" << std::endl;
            return 2;
        }
		outFile.write("MTEM", 4);
		int dummy = 0;
		outFile.write((const char*)&dummy, 4);

        std::string line;
        std::string inst;
        //std::string args[6];

        std::stringstream parser;
        std::string strAlu;
        std::string strR;
        std::string strS;
        std::string strD;
        std::string strAddr;

        size_t opcAlu;
        size_t opcSource;
        size_t opcDestination;
        size_t A, B, D;
        size_t opcJump;
        size_t addr;

        bool useAr;
        bool wasJump;


        _addrPos = 0;
        for (_linePos = 1; inFile.good() /* && (_addrPos < 16) */; ++_linePos) {
            std::getline(inFile, line);
            if (line.empty())
                continue;

            parser.str(line);
            parser.clear();
            parser >> inst;
			// std::cout << line << " <> " << inst << std::endl;

            strAlu.clear();
            strR.clear();
            strS.clear();
            strD.clear();
            strAddr.clear();

            addr = 0;
            wasJump = false;

            if (isLabel(inst)){
                inst.pop_back();
                _labels[inst] = _addrPos;
                //std::cout << "Added new label: \"" << inst << "\" at address " << _addrPos << std::endl;
                continue;
            }

            bool jmode = (inst.back() == 'j');
            if (jmode)
                inst.pop_back();

            if (inst.front() == ';')
                continue;
            if (inst == "nop")
                setNop(opcAlu, opcSource, opcDestination, A, B, D);
            else if (inst.empty())
                continue;
            else {
                try {
                    opcJump = parseJump(inst, useAr);
					wasJump = true;
                    if (useAr)
                        getArg(parser, strAddr, false);
                    setNop(opcAlu, opcSource, opcDestination, A, B, D);
                }
                catch (SyntaxError& e) {
                    opcAlu = parseAlu(inst);
                    getArg(parser, strR, true);
                    getArg(parser, strS, true);
                    getArg(parser, strD, jmode);
                }
                if (wasJump && jmode)
                    throw SyntaxError("Can't use jump operation with jump postfix \"" + inst + "\"", _linePos);
            }
            //*j ... jump, addr
            if (jmode) {
                std::string strJump;
                getArg(parser, strJump, true);
                opcJump = parseJump(strJump, useAr);
                if (useAr)
                    getArg(parser, strAddr, false);
                getArg(parser, strAddr, false);
            }
            else if (!wasJump) {
                opcJump = 0b0010;
                useAr = false;
            }

            if ((!strR.empty()) && (!strS.empty()))
                opcSource = parseSourceRecieve(strR, strS, A, B, D);
            if (!strD.empty())
                opcDestination  = parseDestination(strD, A, B, opcSource);
			

            if (useAr)
                addr = parseAddress(strAddr);

			// std::cout << line << " <> " << inst << std::endl;

            //log(std::cout, opcAlu, opcSource, opcDestination, A, B, D, opcJump, addr);
            writeRom(outFile, opcAlu, opcSource, opcDestination, A, B, D, opcJump, addr);
			// std::cout << line << " <> " << inst << std::endl;

            ++_addrPos;
        }

        // for(; _addrPos < 16; ++_addrPos) {
            // writeNop(outFile);
        // }

        for (auto& x : _labelList) {
            auto it = _labels.find(x.label);
            if (it != _labels.end()) {
				size_t a0 = it->second & 0xF;
				size_t a1 = (it->second >> 4) & 0xF;
				size_t a2 = (it->second >> 8) & 0xF;

				outFile.seekg(x.addrPos * 6 + 10);
				opcJump = outFile.get() & 0xF;
                // outFile.seekp(x.addrPos * 8 + 7);
                outFile.seekp(x.addrPos * 6 + 9);
                // outFile.put((char)it->second);
				outFile.put(concat(a2, a1)); 
				outFile.put(concat(a0, opcJump)); 
			}
            else
                throw SyntaxError("Unknown label \"" + x.label + "\"", x.linePos);
        }
		
		outFile.seekp(4);
		int count = htonl(_addrPos);
		outFile.write((const char*)&count, 4);


        // if (_addrPos > 16)
            // throw SyntaxError("Memory overflow", _linePos);
        return 0;
    }

};


int main(int argc, char* argv[]) {
    std::string outFName;
    if ((argc < 2) || (argc > 3)) {
        std::cout << "Usage: hirou input.asm (output.mte)" << std::endl;
        return 1;
    }
    else if (argc == 2)
        outFName = std::string(argv[1]) + ".mte";
    else
        outFName = argv[2];
    AsmParser parser;
    int result;
    try {
        result = parser.parse(argv[1], outFName);
    }
    catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
        return  2;
    }
    catch (...) {
        std::cerr << "Unknown error" << std::endl;
        return 3;
    }
    return result;
}