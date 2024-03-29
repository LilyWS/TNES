var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var mos6502 = /** @class */ (function () {
    function mos6502() {
        this.flags = {
            C: 1 << 0,
            Z: 1 << 1,
            I: 1 << 2,
            D: 1 << 3,
            B: 1 << 4,
            U: 1 << 5,
            V: 1 << 6,
            N: 1 << 7
        };
        this.a = 0; //accumulator
        this.x = 0; //general use register
        this.y = 0; //general use register
        this.stkp = 0; //points to somewhere on bus
        this.pc = 0; //its the program counter
        this.status = 0; //the status register. keeps track of the flags in a single bit
        this.fetched = 0;
        //these variable will give fetch() addresses, whether they be relative or absolute
        this.absAddr = 0;
        this.relAddr = 0;
        this.opcode = 0; //current opcode being worked with
        this.cycles = 0; //cycles left in opcode
        //A lookup table for all legal opcodes.
        //String name of opcode, opcode function, addressing mode, and then number of clock cycles the opcode takes
        this.lookup = [
            ["BRK", this.BRK, this.IMP, 7], ["ORA", this.ORA, this.IZX, 6], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 3], ["ORA", this.ORA, this.ZP0, 3], ["ASL", this.ASL, this.ZP0, 5], ["???", this.XXX, this.IMP, 5], ["PHP", this.PHP, this.IMP, 3], ["ORA", this.ORA, this.IMM, 2], ["ASL", this.ASL, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["???", this.NOP, this.IMP, 4], ["ORA", this.ORA, this.ABS, 4], ["ASL", this.ASL, this.ABS, 6], ["???", this.XXX, this.IMP, 6],
            ["BPL", this.BPL, this.REL, 2], ["ORA", this.ORA, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 4], ["ORA", this.ORA, this.ZPX, 4], ["ASL", this.ASL, this.ZPX, 6], ["???", this.XXX, this.IMP, 6], ["CLC", this.CLC, this.IMP, 2], ["ORA", this.ORA, this.ABY, 4], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 7], ["???", this.NOP, this.IMP, 4], ["ORA", this.ORA, this.ABX, 4], ["ASL", this.ASL, this.ABX, 7], ["???", this.XXX, this.IMP, 7],
            ["JSR", this.JSR, this.ABS, 6], ["AND", this.AND, this.IZX, 6], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["BIT", this.BIT, this.ZP0, 3], ["AND", this.AND, this.ZP0, 3], ["ROL", this.ROL, this.ZP0, 5], ["???", this.XXX, this.IMP, 5], ["PLP", this.PLP, this.IMP, 4], ["AND", this.AND, this.IMM, 2], ["ROL", this.ROL, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["BIT", this.BIT, this.ABS, 4], ["AND", this.AND, this.ABS, 4], ["ROL", this.ROL, this.ABS, 6], ["???", this.XXX, this.IMP, 6],
            ["BMI", this.BMI, this.REL, 2], ["AND", this.AND, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 4], ["AND", this.AND, this.ZPX, 4], ["ROL", this.ROL, this.ZPX, 6], ["???", this.XXX, this.IMP, 6], ["SEC", this.SEC, this.IMP, 2], ["AND", this.AND, this.ABY, 4], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 7], ["???", this.NOP, this.IMP, 4], ["AND", this.AND, this.ABX, 4], ["ROL", this.ROL, this.ABX, 7], ["???", this.XXX, this.IMP, 7],
            ["RTI", this.RTI, this.IMP, 6], ["EOR", this.EOR, this.IZX, 6], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 3], ["EOR", this.EOR, this.ZP0, 3], ["LSR", this.LSR, this.ZP0, 5], ["???", this.XXX, this.IMP, 5], ["PHA", this.PHA, this.IMP, 3], ["EOR", this.EOR, this.IMM, 2], ["LSR", this.LSR, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["JMP", this.JMP, this.ABS, 3], ["EOR", this.EOR, this.ABS, 4], ["LSR", this.LSR, this.ABS, 6], ["???", this.XXX, this.IMP, 6],
            ["BVC", this.BVC, this.REL, 2], ["EOR", this.EOR, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 4], ["EOR", this.EOR, this.ZPX, 4], ["LSR", this.LSR, this.ZPX, 6], ["???", this.XXX, this.IMP, 6], ["CLI", this.CLI, this.IMP, 2], ["EOR", this.EOR, this.ABY, 4], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 7], ["???", this.NOP, this.IMP, 4], ["EOR", this.EOR, this.ABX, 4], ["LSR", this.LSR, this.ABX, 7], ["???", this.XXX, this.IMP, 7],
            ["RTS", this.RTS, this.IMP, 6], ["ADC", this.ADC, this.IZX, 6], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 3], ["ADC", this.ADC, this.ZP0, 3], ["ROR", this.ROR, this.ZP0, 5], ["???", this.XXX, this.IMP, 5], ["PLA", this.PLA, this.IMP, 4], ["ADC", this.ADC, this.IMM, 2], ["ROR", this.ROR, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["JMP", this.JMP, this.IND, 5], ["ADC", this.ADC, this.ABS, 4], ["ROR", this.ROR, this.ABS, 6], ["???", this.XXX, this.IMP, 6],
            ["BVS", this.BVS, this.REL, 2], ["ADC", this.ADC, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 4], ["ADC", this.ADC, this.ZPX, 4], ["ROR", this.ROR, this.ZPX, 6], ["???", this.XXX, this.IMP, 6], ["SEI", this.SEI, this.IMP, 2], ["ADC", this.ADC, this.ABY, 4], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 7], ["???", this.NOP, this.IMP, 4], ["ADC", this.ADC, this.ABX, 4], ["ROR", this.ROR, this.ABX, 7], ["???", this.XXX, this.IMP, 7],
            ["???", this.NOP, this.IMP, 2], ["STA", this.STA, this.IZX, 6], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 6], ["STY", this.STY, this.ZP0, 3], ["STA", this.STA, this.ZP0, 3], ["STX", this.STX, this.ZP0, 3], ["???", this.XXX, this.IMP, 3], ["DEY", this.DEY, this.IMP, 2], ["???", this.NOP, this.IMP, 2], ["TXA", this.TXA, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["STY", this.STY, this.ABS, 4], ["STA", this.STA, this.ABS, 4], ["STX", this.STX, this.ABS, 4], ["???", this.XXX, this.IMP, 4],
            ["BCC", this.BCC, this.REL, 2], ["STA", this.STA, this.IZY, 6], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 6], ["STY", this.STY, this.ZPX, 4], ["STA", this.STA, this.ZPX, 4], ["STX", this.STX, this.ZPY, 4], ["???", this.XXX, this.IMP, 4], ["TYA", this.TYA, this.IMP, 2], ["STA", this.STA, this.ABY, 5], ["TXS", this.TXS, this.IMP, 2], ["???", this.XXX, this.IMP, 5], ["???", this.NOP, this.IMP, 5], ["STA", this.STA, this.ABX, 5], ["???", this.XXX, this.IMP, 5], ["???", this.XXX, this.IMP, 5],
            ["LDY", this.LDY, this.IMM, 2], ["LDA", this.LDA, this.IZX, 6], ["LDX", this.LDX, this.IMM, 2], ["???", this.XXX, this.IMP, 6], ["LDY", this.LDY, this.ZP0, 3], ["LDA", this.LDA, this.ZP0, 3], ["LDX", this.LDX, this.ZP0, 3], ["???", this.XXX, this.IMP, 3], ["TAY", this.TAY, this.IMP, 2], ["LDA", this.LDA, this.IMM, 2], ["TAX", this.TAX, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["LDY", this.LDY, this.ABS, 4], ["LDA", this.LDA, this.ABS, 4], ["LDX", this.LDX, this.ABS, 4], ["???", this.XXX, this.IMP, 4],
            ["BCS", this.BCS, this.REL, 2], ["LDA", this.LDA, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 5], ["LDY", this.LDY, this.ZPX, 4], ["LDA", this.LDA, this.ZPX, 4], ["LDX", this.LDX, this.ZPY, 4], ["???", this.XXX, this.IMP, 4], ["CLV", this.CLV, this.IMP, 2], ["LDA", this.LDA, this.ABY, 4], ["TSX", this.TSX, this.IMP, 2], ["???", this.XXX, this.IMP, 4], ["LDY", this.LDY, this.ABX, 4], ["LDA", this.LDA, this.ABX, 4], ["LDX", this.LDX, this.ABY, 4], ["???", this.XXX, this.IMP, 4],
            ["CPY", this.CPY, this.IMM, 2], ["CMP", this.CMP, this.IZX, 6], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["CPY", this.CPY, this.ZP0, 3], ["CMP", this.CMP, this.ZP0, 3], ["DEC", this.DEC, this.ZP0, 5], ["???", this.XXX, this.IMP, 5], ["INY", this.INY, this.IMP, 2], ["CMP", this.CMP, this.IMM, 2], ["DEX", this.DEX, this.IMP, 2], ["???", this.XXX, this.IMP, 2], ["CPY", this.CPY, this.ABS, 4], ["CMP", this.CMP, this.ABS, 4], ["DEC", this.DEC, this.ABS, 6], ["???", this.XXX, this.IMP, 6],
            ["BNE", this.BNE, this.REL, 2], ["CMP", this.CMP, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 4], ["CMP", this.CMP, this.ZPX, 4], ["DEC", this.DEC, this.ZPX, 6], ["???", this.XXX, this.IMP, 6], ["CLD", this.CLD, this.IMP, 2], ["CMP", this.CMP, this.ABY, 4], ["NOP", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 7], ["???", this.NOP, this.IMP, 4], ["CMP", this.CMP, this.ABX, 4], ["DEC", this.DEC, this.ABX, 7], ["???", this.XXX, this.IMP, 7],
            ["CPX", this.CPX, this.IMM, 2], ["SBC", this.SBC, this.IZX, 6], ["???", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["CPX", this.CPX, this.ZP0, 3], ["SBC", this.SBC, this.ZP0, 3], ["INC", this.INC, this.ZP0, 5], ["???", this.XXX, this.IMP, 5], ["INX", this.INX, this.IMP, 2], ["SBC", this.SBC, this.IMM, 2], ["NOP", this.NOP, this.IMP, 2], ["???", this.SBC, this.IMP, 2], ["CPX", this.CPX, this.ABS, 4], ["SBC", this.SBC, this.ABS, 4], ["INC", this.INC, this.ABS, 6], ["???", this.XXX, this.IMP, 6],
            ["BEQ", this.BEQ, this.REL, 2], ["SBC", this.SBC, this.IZY, 5], ["???", this.XXX, this.IMP, 2], ["???", this.XXX, this.IMP, 8], ["???", this.NOP, this.IMP, 4], ["SBC", this.SBC, this.ZPX, 4], ["INC", this.INC, this.ZPX, 6], ["???", this.XXX, this.IMP, 6], ["SED", this.SED, this.IMP, 2], ["SBC", this.SBC, this.ABY, 4], ["NOP", this.NOP, this.IMP, 2], ["???", this.XXX, this.IMP, 7], ["???", this.NOP, this.IMP, 4], ["SBC", this.SBC, this.ABX, 4], ["INC", this.INC, this.ABX, 7], ["???", this.XXX, this.IMP, 7],
        ];
    }
    mos6502.prototype.connectBus = function (bus) {
        this.bus = bus;
    };
    mos6502.prototype.write = function (addr, val) {
        this.bus.cpuWrite(addr, val);
    };
    mos6502.prototype.read = function (addr) {
        return this.bus.cpuRead(addr);
    };
    mos6502.prototype.getFlag = function (i) {
        return ((this.status & i) ? 1 : 0);
    };
    mos6502.prototype.setFlag = function (i, i2) {
        if (i2) {
            this.status |= i;
        }
        else {
            this.status &= ~i;
        }
    };
    //defining all 12 addressing modes for the cpu
    // IMP(){}; IMM(){};	
    // ZP0(){}; ZPX(){};	
    // ZPY(){}; REL(){};
    // ABS(){}; ABX(){};	
    // ABY(){}; IND(){};	
    // IZX(){}; IZY(){};
    //op codes, 56 total. Only emulating legal ones
    // ADC(){};	AND(){};	ASL(){};	BCC(){};
    // BCS(){};	BEQ(){};	BIT(){};	BMI(){};
    // BNE(){};	BPL(){};	BRK(){};	BVC(){};
    // BVS(){};	CLC(){};	CLD(){};	CLI(){};
    // CLV(){};	CMP(){};	CPX(){};	CPY(){};
    // DEC(){};	DEX(){};	DEY(){};	EOR(){};
    // INC(){};	INX(){};	INY(){};	JMP(){};
    // JSR(){};	LDA(){};	LDX(){};	LDY(){};
    // LSR(){};	NOP(){};	ORA(){};	PHA(){};
    // PHP(){};	PLA(){};	PLP(){};	ROL(){};
    // ROR(){};	RTI(){};	RTS(){};	SBC(){};
    // SEC(){};	SED(){};	SEI(){};	STA(){};
    // STX(){};	STY(){};	TAX(){};	TAY(){};
    // TSX(){};	TXA(){};	TXS(){};	TYA(){};
    // XXX(){}; //this function will catch all unofficial opcodes and as a NOP call
    // these three functions are asynchronous. they are activated externally and will change the state of the cpu. 
    mos6502.prototype.reset = function () {
        this.absAddr = 0xFFFC;
        var lo = this.read(this.absAddr + 0);
        var hi = this.read(this.absAddr + 1);
        this.pc = (hi << 8) | lo;
        this.a = 0;
        this.x = 0;
        this.y = 0;
        this.stkp = 0xFD;
        this.status = 0x00;
        this.relAddr = 0x0000;
        this.absAddr = 0x0000;
        this.fetched = 0x00;
        this.cycles = 8;
    };
    ;
    mos6502.prototype.irq = function () {
        if (this.getFlag(this.flags.I) == 0) {
            this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF);
            this.stkp--;
            this.write(0x0100 + this.stkp, this.pc & 0x00FF);
            this.stkp--;
            this.setFlag(this.flags.B, 0);
            this.setFlag(this.flags.U, 1);
            this.setFlag(this.flags.I, 1);
            this.write(0x0100 + this.stkp, this.status);
            this.stkp--;
            this.absAddr = 0xFFFE;
            var lo = this.read(this.absAddr + 0);
            var hi = this.read(this.absAddr + 1);
            this.pc = (hi << 8) | lo;
            this.cycles = 7;
        }
    };
    ;
    mos6502.prototype.nmi = function () {
        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF);
        this.stkp--;
        this.write(0x0100 + this.stkp, this.pc & 0x00FF);
        this.stkp--;
        this.setFlag(this.flags.B, 0);
        this.setFlag(this.flags.U, 1);
        this.setFlag(this.flags.I, 1);
        this.write(0x0100 + this.stkp, this.status);
        this.stkp--;
        this.absAddr = 0xFFFA;
        var lo = this.read(this.absAddr + 0);
        var hi = this.read(this.absAddr + 1);
        this.pc = (hi << 8) | lo;
        this.cycles = 8;
    };
    ;
    mos6502.prototype.clock = function () {
        if (this.cycles == 0) { //if its time for next opcode
            //read the current opcode and increment the programcounter
            this.opcode = this.read(this.pc);
            this.pc++;
            var opc = this.lookup[this.opcode]; //stores the opcode we need
            //console.log(opc[0]);
            this.cycles = opc[3];
            var ac1 = opc[2].bind(this)(); // find out if both methods request additional cycles
            var ac2 = opc[1].bind(this)();
            this.cycles += (ac1 & ac2);
        }
        ;
        this.cycles--;
    };
    ;
    //adressing modes
    mos6502.prototype.IMP = function () {
        this.fetched = this.a;
        return 0;
    };
    mos6502.prototype.IMM = function () {
        this.absAddr = this.pc++;
        return 0;
    };
    mos6502.prototype.ZP0 = function () {
        this.absAddr = this.read(this.pc);
        this.pc++;
        this.absAddr &= 0x00ff;
        return 0;
    };
    mos6502.prototype.ZPX = function () {
        this.absAddr = this.read(this.pc) + this.x;
        this.pc++;
        this.absAddr &= 0x00ff;
        return 0;
    };
    mos6502.prototype.ZPY = function () {
        this.absAddr = this.read(this.pc) + this.y;
        this.pc++;
        this.absAddr &= 0x00ff;
        return 0;
    };
    mos6502.prototype.ABS = function () {
        var lo = this.read(this.pc);
        this.pc++;
        var hi = this.read(this.pc);
        this.pc++;
        this.absAddr = (hi << 8) | lo;
        return 0;
    };
    mos6502.prototype.ABX = function () {
        var lo = this.read(this.pc);
        this.pc++;
        var hi = this.read(this.pc);
        this.pc++;
        this.absAddr = (hi << 8) | lo;
        this.absAddr += this.x;
        if ((this.absAddr & 0xFF00) != (hi << 8)) { // if we moved a page when adding x, we need to requst time
            return 1;
        }
        else {
            return 0;
        }
    };
    mos6502.prototype.ABY = function () {
        var lo = this.read(this.pc);
        this.pc++;
        var hi = this.read(this.pc);
        this.pc++;
        this.absAddr = (hi << 8) | lo;
        this.absAddr += this.y;
        if ((this.absAddr & 0xFF00) != (hi << 8)) { // if we moved a page when adding x, we need to requst time
            return 1;
        }
        else {
            return 0;
        }
    };
    mos6502.prototype.IND = function () {
        var lo = this.read(this.pc);
        this.pc++;
        var hi = this.read(this.pc);
        this.pc++;
        var ptr = (hi << 8) | lo;
        if (lo == 0x00FF) { //simulate page boundary bug
            this.absAddr = (this.read(ptr & 0xFF00) << 8) | this.read(ptr + 0);
        }
        else { //otherwise behave normally
            this.absAddr = (this.read(ptr + 1) << 8) | this.read(ptr + 0);
        }
        return 0;
    };
    mos6502.prototype.IZX = function () {
        var t = this.read(this.pc); //temp variable
        this.pc++;
        var lo = this.read((t + this.x) & 0x00FF);
        var hi = this.read((t + this.x + 1) & 0x00FF);
        this.absAddr = (hi << 8) | lo;
        return 0;
    };
    mos6502.prototype.IZY = function () {
        var t = this.read(this.pc); //temp variable
        this.pc++;
        var lo = this.read(t & 0x00FF);
        var hi = this.read((t + 1) & 0x00FF);
        this.absAddr = (hi << 8) | lo;
        this.absAddr += this.y;
        if ((this.absAddr & 0xFF00) != (hi << 8)) {
            return 1;
        }
        else {
            return 0;
        }
    };
    mos6502.prototype.REL = function () {
        this.relAddr = this.read(this.pc);
        this.pc++;
        if (this.relAddr & 0x80) { //checking if byte 7 is equal to 1. if it is we jump backwards instead of forwards
            this.relAddr -= 256;
        }
        return 0;
    };
    //instructions
    //helper function that will grab data and store it in the fetched variable
    mos6502.prototype.fetch = function () {
        if (this.lookup[this.opcode][2] != this.IMP) { //lily ensure this works asap
            this.fetched = this.read(this.absAddr);
        }
        return this.fetched;
    };
    mos6502.prototype.ADC = function () {
        this.fetch();
        var temp = this.a + this.fetched + this.getFlag(this.flags.C); //POTENTIAL BUG: carry bit might not be accurate here
        this.setFlag(this.flags.C, temp > 255);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0);
        this.setFlag(this.flags.N, temp & 0x80);
        //set the v flag based on signed overflow
        this.setFlag(this.flags.V, (~(this.a ^ this.fetched) & this.a ^ temp) & 0x0080);
        this.a = temp & 0x00FF;
        return 1;
    };
    mos6502.prototype.SBC = function () {
        this.fetch();
        var value = this.fetched ^ 0x0FF;
        var temp = this.a + value + this.getFlag(this.flags.C);
        this.setFlag(this.flags.C, temp & 0xFF00);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0);
        this.setFlag(this.flags.N, temp & 0x80);
        this.a = temp & 0x00FF;
        return 1;
    };
    mos6502.prototype.AND = function () {
        this.fetch();
        this.a = this.a & this.fetched;
        this.setFlag(this.flags.Z, this.a == 0); //set zero flag if a is 0
        this.setFlag(this.flags.constructor, this.a & 0x80);
        return 1;
    };
    // Instruction: Arithmetic Shift Left
    // Function:    A = C <- (A << 1) <- 0
    // Flags Out:   N, Z, C
    mos6502.prototype.ASL = function () {
        this.fetch();
        var temp = this.fetched << 1;
        this.setFlag(this.flags.C, (temp & 0xFF00) > 0);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x00);
        this.setFlag(this.flags.N, (temp & 0x80));
        return 1;
    };
    // Instruction: Branch if Carry Clear
    // Function:    if(C == 0) pc = address 
    mos6502.prototype.BCC = function () {
        if (this.getFlag(this.flags.C) == 0) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    mos6502.prototype.BCS = function () {
        if (this.getFlag(this.flags.C) == 1) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    mos6502.prototype.BEQ = function () {
        if (this.getFlag(this.flags.Z) == 1) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    mos6502.prototype.BIT = function () {
        this.fetch();
        var temp = this.a & this.fetched;
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x00);
        this.setFlag(this.flags.N, (1 << 7));
        this.setFlag(this.flags.V, (1 << 6));
        return 0;
    };
    // Instruction: Branch if Negative
    // Function:    if(N == 1) pc = address
    mos6502.prototype.BMI = function () {
        if (this.getFlag(this.flags.N) == 1) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    // Instruction: Branch if Not Equal
    // Function:    if(Z == 0) pc = address
    mos6502.prototype.BNE = function () {
        if (this.getFlag(this.flags.Z) == 0) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    // Instruction: Branch if Positive
    // Function:    if(N == 0) pc = address
    mos6502.prototype.BPL = function () {
        if (this.getFlag(this.flags.N) == 0) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    // Instruction: Break
    // Function:    Program Sourced Interrupt
    mos6502.prototype.BRK = function () {
        this.pc++;
        this.setFlag(this.flags.I, 1);
        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF);
        this.stkp--;
        this.write(0x0100 + this.stkp, this.pc & 0x00FF);
        this.stkp--;
        this.setFlag(this.flags.B, 1);
        this.write(0x0100 + this.stkp, this.status);
        this.stkp--;
        this.setFlag(this.flags.B, 0);
        this.pc = this.read(0xFFFE) | this.read(0xFFFF) << 8;
        return 0;
    };
    // Instruction: Branch if Overflow Clear
    // Function:    if(V == 0) pc = address
    mos6502.prototype.BVC = function () {
        if (this.getFlag(this.flags.V) == 0) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    // Instruction: Branch if Overflow Set
    // Function:    if(V == 1) pc = address
    mos6502.prototype.BVS = function () {
        if (this.getFlag(this.flags.V) == 1) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;
            if ((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }
            this.pc = this.absAddr;
        }
        return 0;
    };
    mos6502.prototype.CLC = function () {
        this.setFlag(this.flags.C, false);
        return 0;
    };
    mos6502.prototype.CLD = function () {
        this.setFlag(this.flags.D, false);
        return 0;
    };
    mos6502.prototype.CLV = function () {
        this.setFlag(this.flags.V, false);
        return 0;
    };
    // Instruction: Disable Interrupts / Clear Interrupt Flag
    // Function:    I = 0
    mos6502.prototype.CLI = function () {
        this.setFlag(this.flags.I, false);
        return 0;
    };
    // Instruction: Compare Accumulator
    // Function:    C <- A >= M      Z <- (A - M) == 0
    // Flags Out:   N, C, Z
    mos6502.prototype.CMP = function () {
        this.fetch();
        var temp = this.a - this.fetched;
        this.setFlag(this.flags.C, this.a >= this.fetched);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        return 1;
    };
    //compare x register
    mos6502.prototype.CPX = function () {
        this.fetch();
        var temp = this.x - this.fetched;
        this.setFlag(this.flags.C, this.x >= this.fetched);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        return 0;
    };
    //compare y register
    mos6502.prototype.CPY = function () {
        this.fetch();
        var temp = this.y - this.fetched;
        this.setFlag(this.flags.C, this.y >= this.fetched);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        return 0;
    };
    // Instruction: Decrement Value at Memory Location
    // Function:    M = M - 1
    // Flags Out:   N, Z
    mos6502.prototype.DEC = function () {
        this.fetch();
        var temp = this.fetched - 1;
        this.write(this.absAddr, temp & 0x00FF);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        return 0;
    };
    mos6502.prototype.DEX = function () {
        this.x--;
        this.setFlag(this.flags.Z, this.x == 0x00);
        this.setFlag(this.flags.N, this.x & 0x80);
        return 0;
    };
    mos6502.prototype.DEY = function () {
        this.y--;
        this.setFlag(this.flags.Z, this.y == 0x00);
        this.setFlag(this.flags.N, this.y & 0x80);
        return 0;
    };
    // Instruction: Bitwise Logic XOR
    // Function:    A = A xor M
    // Flags Out:   N, Z
    mos6502.prototype.EOR = function () {
        this.fetch();
        this.a = this.a ^ this.fetched;
        this.setFlag(this.flags.Z, this.a == 0x00);
        this.setFlag(this.flags.N, this.a & 0x80);
        return 1;
    };
    // Instruction: Increment Value at Memory Location
    // Function:    M = M + 1
    // Flags Out:   N, Z
    mos6502.prototype.INC = function () {
        this.fetch();
        var temp = this.fetched + 1;
        this.write(this.absAddr, temp & 0x00FF);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        return 0;
    };
    mos6502.prototype.INX = function () {
        this.x++;
        this.setFlag(this.flags.Z, this.x == 0x00);
        this.setFlag(this.flags.N, this.x & 0x80);
        return 0;
    };
    mos6502.prototype.INY = function () {
        this.y++;
        this.setFlag(this.flags.Z, this.y == 0x00);
        this.setFlag(this.flags.N, this.y & 0x80);
        return 0;
    };
    // Instruction: Jump To Location
    // Function:    pc = address
    mos6502.prototype.JMP = function () {
        this.pc = this.absAddr;
        return 0;
    };
    // Instruction: Jump To Sub-Routine
    // Function:    Push current pc to stack, pc = address
    mos6502.prototype.JSR = function () {
        this.pc--;
        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF);
        this.stkp--;
        this.write(0x0100 + this.stkp, this.pc & 0x00FF);
        this.stkp--;
        this.pc = this.absAddr;
        return 0;
    };
    // Instruction: Load The Accumulator
    // Function:    A = M
    // Flags Out:   N, Z
    mos6502.prototype.LDA = function () {
        this.fetch();
        this.a = this.fetched;
        this.setFlag(this.flags.Z, this.a == 0x00);
        this.setFlag(this.flags.N, this.a & 0x80);
        return 1;
    };
    mos6502.prototype.LDX = function () {
        this.fetch();
        this.x = this.fetched;
        this.setFlag(this.flags.Z, this.x == 0x00);
        this.setFlag(this.flags.N, this.x & 0x80);
        return 1;
    };
    mos6502.prototype.LDY = function () {
        this.fetch();
        this.y = this.fetched;
        this.setFlag(this.flags.Z, this.y == 0x00);
        this.setFlag(this.flags.N, this.y & 0x80);
        return 1;
    };
    mos6502.prototype.LSR = function () {
        this.fetch();
        this.setFlag(this.flags.C, this.fetched & 0x0001);
        var temp = this.fetched >> 1;
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        if (this.lookup[this.opcode][2] == this.IMP) {
            this.a = temp & 0x00FF;
        }
        else {
            this.write(this.absAddr, temp & 0x00FF);
        }
        return 0;
    };
    mos6502.prototype.NOP = function () {
        switch (this.opcode) {
        } //TODO: Implement NOP timings
        return 0;
    };
    // Instruction: Bitwise Logic OR
    // Function:    A = A | M
    // Flags Out:   N, Z
    mos6502.prototype.ORA = function () {
        this.fetch();
        this.a = this.a | this.fetched;
        this.setFlag(this.flags.Z, this.a == 0x00);
        this.setFlag(this.flags.N, this.a & 0x80);
        return 1;
    };
    mos6502.prototype.PHA = function () {
        this.write(0x0100 + this.stkp, this.a);
        this.stkp--;
        return 0;
    };
    // Instruction: Push Status Register to Stack
    // Function:    status -> stack
    // Note:        Break flag is set to 1 before push
    mos6502.prototype.PHP = function () {
        this.write(0x0100 + this.stkp, this.status | this.flags.B);
        this.setFlag(this.flags.B, 0);
        this.setFlag(this.flags.U, 0);
        this.stkp--;
        return 0;
    };
    mos6502.prototype.PLA = function () {
        this.stkp++;
        this.a = this.read(0x0100 + this.stkp);
        this.setFlag(this.flags.Z, this.a == 0x00);
        this.setFlag(this.flags.N, this.a & 0x80);
        return 0;
    };
    // Instruction: Pop Status Register off Stack
    // Function:    Status <- stack
    mos6502.prototype.PLP = function () {
        this.stkp++;
        this.status = this.read(0x0100 + this.stkp);
        this.setFlag(this.flags.U, 1);
        return 0;
    };
    mos6502.prototype.ROL = function () {
        this.fetch();
        var temp = (this.fetched << 1 | this.getFlag(this.flags.C));
        this.setFlag(this.flags.C, temp & 0xFF00);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
        this.setFlag(this.flags.N, temp & 0x0080);
        if (this.lookup[this.opcode][2] == this.IMP) {
            this.a = temp & 0x00FF;
        }
        else {
            this.write(this.absAddr, temp & 0x00FF);
        }
        return 0;
    };
    mos6502.prototype.ROR = function () {
        this.fetch();
        var temp = (this.fetched >> 1) | (this.getFlag(this.flags.C) << 7);
        this.setFlag(this.flags.C, this.fetched & 0x01);
        this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x00);
        this.setFlag(this.flags.N, temp & 0x0080);
        if (this.lookup[this.opcode][2] == this.IMP) {
            this.a = temp & 0x00FF;
        }
        else {
            this.write(this.absAddr, temp & 0x00FF);
        }
        return 0;
    };
    mos6502.prototype.RTI = function () {
        this.stkp++;
        this.status = this.read(0x0100 + this.stkp);
        this.status &= ~this.getFlag(this.flags.B);
        this.status &= ~this.getFlag(this.flags.U);
        this.stkp++;
        this.pc = this.read(0x0100 + this.stkp);
        this.stkp++;
        this.pc |= this.read(0x0100 + this.stkp) << 8;
        return 0;
    };
    mos6502.prototype.RTS = function () {
        this.stkp++;
        this.pc = this.read(0x0100 + this.stkp);
        this.stkp++;
        this.pc |= this.read(0x0100 + this.stkp) << 8;
        this.pc++;
        return 0;
    };
    mos6502.prototype.SEC = function () {
        this.setFlag(this.flags.C, true);
        return 0;
    };
    mos6502.prototype.SED = function () {
        this.setFlag(this.flags.D, true);
        return 0;
    };
    mos6502.prototype.SEI = function () {
        this.setFlag(this.flags.I, true);
        return 0;
    };
    mos6502.prototype.STA = function () {
        this.write(this.absAddr, this.a);
        return 0;
    };
    mos6502.prototype.STX = function () {
        this.write(this.absAddr, this.x);
        return 0;
    };
    mos6502.prototype.STY = function () {
        this.write(this.absAddr, this.y);
        return 0;
    };
    // Instruction: Transfer Accumulator to X Register
    // Function:    X = A
    // Flags Out:   N, Z
    mos6502.prototype.TAX = function () {
        this.x = this.a;
        this.setFlag(this.flags.Z, this.x == 0x00);
        this.setFlag(this.flags.N, this.x & 0x80);
        return 0;
    };
    mos6502.prototype.TAY = function () {
        this.y = this.a;
        this.setFlag(this.flags.Z, this.y == 0x00);
        this.setFlag(this.flags.N, this.y & 0x80);
        return 0;
    };
    mos6502.prototype.TSX = function () {
        this.x = this.stkp;
        this.setFlag(this.flags.Z, this.x == 0x00);
        this.setFlag(this.flags.N, this.x & 0x80);
        return 0;
    };
    mos6502.prototype.TXA = function () {
        this.a = this.x;
        this.setFlag(this.flags.Z, this.a == 0x00);
        this.setFlag(this.flags.N, this.a & 0x80);
        return 0;
    };
    mos6502.prototype.TXS = function () {
        this.stkp = this.x;
        return 0;
    };
    mos6502.prototype.TYA = function () {
        this.a = this.y;
        this.setFlag(this.flags.Z, this.a == 0x00);
        this.setFlag(this.flags.N, this.a & 0x80);
        return 0;
    };
    //capture all illegal opcodes
    mos6502.prototype.XXX = function () {
        return 0;
    };
    return mos6502;
}());
var PPU = /** @class */ (function () {
    function PPU() {
        this.paletteTable = [0]; //stores 32 bytes
        //set the pallete table colors
        this.paletteTable[0x00] = "rgb(84, 84, 84)";
        this.paletteTable[0x01] = "rgb(0, 30, 116)";
        this.paletteTable[0x02] = "rgb(8, 16, 144)";
        this.paletteTable[0x03] = "rgb(48, 0, 136)";
        this.paletteTable[0x04] = "rgb(68, 0, 100)";
        this.paletteTable[0x05] = "rgb(92, 0, 48)";
        this.paletteTable[0x06] = "rgb(84, 4, 0)";
        this.paletteTable[0x07] = "rgb(60, 24, 0)";
        this.paletteTable[0x08] = "rgb(32, 42, 0)";
        this.paletteTable[0x09] = "rgb(8, 58, 0)";
        this.paletteTable[0x0A] = "rgb(0, 64, 0)";
        this.paletteTable[0x0B] = "rgb(0, 60, 0)";
        this.paletteTable[0x0C] = "rgb(0, 50, 60)";
        this.paletteTable[0x0D] = "rgb(0, 0, 0)";
        this.paletteTable[0x0E] = "rgb(0, 0, 0)";
        this.paletteTable[0x0F] = "rgb(0, 0, 0)";
        this.paletteTable[0x10] = "rgb(152, 150, 152)";
        this.paletteTable[0x11] = "rgb(8, 76, 196)";
        this.paletteTable[0x12] = "rgb(48, 50, 236)";
        this.paletteTable[0x13] = "rgb(92, 30, 228)";
        this.paletteTable[0x14] = "rgb(136, 20, 176)";
        this.paletteTable[0x15] = "rgb(160, 20, 100)";
        this.paletteTable[0x16] = "rgb(152, 34, 32)";
        this.paletteTable[0x17] = "rgb(120, 60, 0)";
        this.paletteTable[0x18] = "rgb(84, 90, 0)";
        this.paletteTable[0x19] = "rgb(40, 114, 0)";
        this.paletteTable[0x1A] = "rgb(8, 124, 0)";
        this.paletteTable[0x1B] = "rgb(0, 118, 40)";
        this.paletteTable[0x1C] = "rgb(0, 102, 120)";
        this.paletteTable[0x1D] = "rgb(0, 0, 0)";
        this.paletteTable[0x1E] = "rgb(0, 0, 0)";
        this.paletteTable[0x1F] = "rgb(0, 0, 0)";
        this.paletteTable[0x20] = "rgb(236, 238, 236)";
        this.paletteTable[0x21] = "rgb(76, 154, 236)";
        this.paletteTable[0x22] = "rgb(120, 124, 236)";
        this.paletteTable[0x23] = "rgb(176, 98, 236)";
        this.paletteTable[0x24] = "rgb(228, 84, 236)";
        this.paletteTable[0x25] = "rgb(236, 88, 180)";
        this.paletteTable[0x26] = "rgb(236, 106, 100)";
        this.paletteTable[0x27] = "rgb(212, 136, 32)";
        this.paletteTable[0x28] = "rgb(160, 170, 0)";
        this.paletteTable[0x29] = "rgb(116, 196, 0)";
        this.paletteTable[0x2A] = "rgb(76, 208, 32)";
        this.paletteTable[0x2B] = "rgb(56, 204, 108)";
        this.paletteTable[0x2C] = "rgb(56, 180, 204)";
        this.paletteTable[0x2D] = "rgb(60, 60, 60)";
        this.paletteTable[0x2E] = "rgb(0, 0, 0)";
        this.paletteTable[0x2F] = "rgb(0, 0, 0)";
        this.paletteTable[0x30] = "rgb(236, 238, 236)";
        this.paletteTable[0x31] = "rgb(168, 204, 236)";
        this.paletteTable[0x32] = "rgb(188, 188, 236)";
        this.paletteTable[0x33] = "rgb(212, 178, 236)";
        this.paletteTable[0x34] = "rgb(236, 174, 236)";
        this.paletteTable[0x35] = "rgb(236, 174, 212)";
        this.paletteTable[0x36] = "rgb(236, 180, 176)";
        this.paletteTable[0x37] = "rgb(228, 196, 144)";
        this.paletteTable[0x38] = "rgb(204, 210, 120)";
        this.paletteTable[0x39] = "rgb(180, 222, 120)";
        this.paletteTable[0x3A] = "rgb(168, 226, 144)";
        this.paletteTable[0x3B] = "rgb(152, 226, 180)";
        this.paletteTable[0x3C] = "rgb(160, 214, 228)";
        this.paletteTable[0x3D] = "rgb(160, 162, 160)";
        this.paletteTable[0x3E] = "rgb(0, 0, 0)";
        this.paletteTable[0x3F] = "rgb(0, 0, 0)";
        this.nameTable = [[0]]; //one name table is 1kb. nes can store 2;
        this.display = document.querySelector("#screen");
        this.context = this.display.getContext("2d");
        this.scanline = 0;
        this.cycle = 0;
        this.frameComplete = false;
        for (var i = 0; i < 2; i++) {
            this.nameTable[i] = [0];
            for (var j = 0; j < 1024; j++) {
                this.nameTable[i][j] = 0;
            }
        }
    }
    PPU.prototype.cpuRead = function (addr) {
        var data = 0x00;
        switch (addr) {
            case 0x0000: // Control
                break;
            case 0x0001: // Mask
                break;
            case 0x0002: // Status
                break;
            case 0x0003: // OAM Address
                break;
            case 0x0004: // OAM Data
                break;
            case 0x0005: // Scroll
                break;
            case 0x0006: // PPU Address
                break;
            case 0x0007: // PPU Data
                break;
        }
        return data;
    };
    ;
    PPU.prototype.cpuWrite = function (addr, val) {
        switch (addr) {
            case 0x0000: // Control
                break;
            case 0x0001: // Mask
                break;
            case 0x0002: // Status
                break;
            case 0x0003: // OAM Address
                break;
            case 0x0004: // OAM Data
                break;
            case 0x0005: // Scroll
                break;
            case 0x0006: // PPU Address
                break;
            case 0x0007: // PPU Data
                break;
        }
    };
    ;
    PPU.prototype.ppuRead = function (addr) {
        var data = 0x00;
        addr &= 0x3FFF;
        if (this.cartridge.ppuRead(addr)) {
        }
        return data;
    };
    ;
    PPU.prototype.ppuWrite = function (addr, val) {
        ;
        addr &= 0x3FFF;
        if (this.cartridge.ppuWrite(addr, val)) {
        }
    };
    PPU.prototype.connectCartridge = function (cart) {
        this.cartridge = cart;
    };
    PPU.prototype.clock = function () {
        //draw at current location
        this.context.fillStyle = this.paletteTable[0x24];
        this.context.fillRect((this.cycle) * 800 / 342, this.scanline * 700 / 262, 800 / 342, 700 / 262);
        //advance cycle and scanline accordlingly
        this.cycle++;
        if (this.cycle >= 341) {
            this.cycle = 0;
            this.scanline++;
            if (this.scanline >= 261) {
                console.log("finito")
                this.scanline = -1;
                this.frameComplete = true;
            }
        }
    };
    return PPU;
}());
var Mapper = /** @class */ (function () {
    function Mapper(pBanks, cBanks) {
        this.PRGBanks = pBanks;
        this.CHRBanks = cBanks;
    }
    //virtual functions to be overwritten in extensions of this class
    Mapper.prototype.cpuMapRead = function (addr) { };
    ;
    Mapper.prototype.cpuMapWrite = function (addr) { };
    ;
    Mapper.prototype.ppuMapRead = function (addr) { };
    ;
    Mapper.prototype.ppuMapWrite = function (addr) { };
    ;
    return Mapper;
}());
var Mapper_000 = /** @class */ (function (_super) {
    __extends(Mapper_000, _super);
    function Mapper_000(pBanks, cBanks) {
        return _super.call(this, pBanks, cBanks) || this;
    }
    Mapper_000.prototype.cpuMapRead = function (addr) {
        if (addr >= 0x8000 && addr <= 0xFFFF) {
            var mappedAddr = addr & (this.PRGBanks > 1 ? 0x7FFF : 0x3FFF);
            return mappedAddr;
        }
        return -1;
    };
    ;
    Mapper_000.prototype.cpuMapWrite = function (addr) {
        ;
        if (addr >= 0x8000 && addr <= 0xFFFF) {
            var mappedAddr = addr & (this.PRGBanks > 1 ? 0x7FFF : 0x3FFF);
            return mappedAddr;
        }
        return -1;
    };
    Mapper_000.prototype.ppuMapRead = function (addr) {
        if (addr >= 0x0000 && addr <= 0x1FFF) {
            var mappedAddr = addr;
            return mappedAddr;
        }
        return -1;
    };
    ;
    Mapper_000.prototype.ppuMapWrite = function (addr) {
        return -1;
    };
    ;
    return Mapper_000;
}(Mapper));
var Cartridge = /** @class */ (function () {
    function Cartridge() {
        this.header = {
            name: "",
            prgRomChunks: 0,
            chrRomChunks: 0,
            mapper1: 0,
            mapper2: 0,
            prgRamSize: 0,
            tvSystem1: 0,
            tvSystem2: 0,
            unused: ""
        };
        this.PRGBanks = 0;
        this.PRGMem = [0];
        this.CHRBanks = 0;
        this.CHRMem = [0];
        this.mirror = ""; //handles mirroring for the nametable which is responsible for displaying backgrounds
    }
    Cartridge.prototype.initCartridge = function (data, bus) {
        //console.log(this);
        bus.insertCartridge(this);
        var cartridgePntr = 16; //we will init header using a for loop but after that we will rely on this var to point to where we want to read from the cartride;
        //get header
        for (var i = 0; i < 16; i++) {
            if (i < 3) { //set name
                this.header.name += data[i].toString(16);
            }
            else if (i == 4) {
                this.header.prgRomChunks = data[i];
            }
            else if (i == 5) {
                this.header.chrRomChunks = data[i];
            }
            else if (i == 6) {
                this.header.mapper1 = data[i];
            }
            else if (i == 7) {
                this.header.mapper2 = data[i];
            }
            else if (i == 8) {
                this.header.prgRamSize = data[i];
            }
            else if (i == 9) {
                this.header.tvSystem1 = data[i];
            }
            else if (i == 10) {
                this.header.tvSystem2 = data[i];
            }
            else if (i > 10) {
                this.header.unused += data[i].toString(16);
            }
        }
        //console.log(this.header);
        if (this.header.mapper1 & 0x04) { //checks if "trainer" exists. if it does we skip past it;
            cartridgePntr += 512; //Lily this may be an inanaccurate please check back if things dont work
        }
        this.mirror = this.header.mapper1 & 0x01 ? "vertical" : "horizontal";
        //get mapper id. mapper 2 stores the upper nybble and mapper 1 stores the lower nyble of the id
        this.mapperID = ((this.header.mapper2 >> 4) << 4) | (this.header.mapper1 >> 4);
        //get type of our iNES file. for now we will only deal with type1
        var nFileType = 1;
        if (nFileType == 0) {
        }
        if (nFileType == 1) {
            this.PRGBanks = this.header.prgRomChunks; //get size of prg memory and then read it in from the rom
            for (var i = 0; i < this.PRGBanks * 16384; i++) {
                this.PRGMem[i] = data[cartridgePntr];
                cartridgePntr++;
            }
            this.CHRBanks = this.header.chrRomChunks; //get size of prg memory and then read it in from the rom
            for (var i = 0; i < this.CHRBanks * 8192; i++) {
                this.CHRMem[i] = data[cartridgePntr];
                cartridgePntr++;
            }
            //console.log(this);
        }
        if (nFileType == 2) {
        }
        switch (this.mapperID) {
            case 0:
                this.mapper = new Mapper_000(this.header.prgRomChunks, this.header.chrRomChunks);
                break;
        }
    };
    //will hijack read/writes from the bus/ppu if the addr is within the cartridges range
    Cartridge.prototype.cpuRead = function (addr) {
        var mappedAddr = this.mapper.cpuMapRead(addr);
        var data = -1;
        if (mappedAddr > -1) {
            data = this.PRGMem[mappedAddr];
        }
        //console.log(data);
        return data;
    };
    ;
    Cartridge.prototype.cpuWrite = function (addr, val) {
        var mappedAddr = this.mapper.cpuMapRead(addr);
        if (mappedAddr > -1) {
            this.PRGMem[mappedAddr] = val;
            return true;
        }
        else {
            return false;
        }
    };
    ;
    Cartridge.prototype.ppuRead = function (addr) {
        var mappedAddr = this.mapper.ppuMapRead(addr);
        var data = -1;
        if (mappedAddr > -1) {
            data = this.CHRMem[mappedAddr];
        }
        return data;
    };
    ;
    Cartridge.prototype.ppuWrite = function (addr, val) {
        return -1;
    };
    ;
    return Cartridge;
}());
var Bus = /** @class */ (function () {
    function Bus() {
        this.cpuRam = [0]; //2kb ram for the CPU
        this.clockCounter = 0;
        for (var i = 0; i < 2048; i++) {
            this.cpuRam[i] = 0;
        }
    }
    Bus.prototype.cpuWrite = function (addr, val) {
        if (this.cartridge.cpuWrite(addr, val)) {
            //check if cpu write is in cartridge address range
        }
        else if (addr >= 0x0000 && addr <= 0x1FFF) {
            this.cpuRam[addr & 0x07FF] = val;
        }
        else if (addr >= 0x2000 && addr <= 0x3FFF) {
            this.ppu.cpuWrite(addr & 0x0007, val);
        }
    };
    Bus.prototype.cpuRead = function (addr) {
        var data = 0x00;
        if (this.cartridge.cpuRead(addr) > -1) {
            //check if cpu read is in cartridge address range
            data = this.cartridge.cpuRead(addr);
            //console.log("bus:", data);
        }
        else if (addr >= 0x0000 && addr <= 0x1FFF) {
            data = this.cpuRam[addr & 0x07FF]; // implements mirroring for the 8kb addressable range of the cpu ram 
        }
        else if (addr >= 0x2000 && addr <= 0x3FFF) {
            data = this.ppu.cpuRead(addr & 0x0007);
        }
        return data;
    };
    //system functions
    Bus.prototype.insertCartridge = function (cart) {
        this.cartridge = cart;
        this.ppu.connectCartridge(cart);
    };
    Bus.prototype.connectPPU = function (ppu) {
        this.ppu = ppu;
    };
    Bus.prototype.connectCPU = function (cpu) {
        this.cpu = cpu;
    };
    Bus.prototype.reset = function () {
        this.cpu.reset();
        this.clockCounter = 0;
    };
    Bus.prototype.clock = function () {
        this.ppu.clock();
        if (this.clockCounter % 3 == 0) { //cpu clock runs 3 times slower than ppu;
            this.cpu.clock();
        }
        this.clockCounter++;
    };
    return Bus;
}());
var cpu = new mos6502();
var ppu = new PPU();
var bus = new Bus();
var cart = new Cartridge();
cpu.connectBus(bus);
bus.connectPPU(ppu);
bus.connectCPU(cpu);
//console.log(cpu.pc);
//console.log(cpu.pc);
var rom = "A20A8E0000A2038E0100AC0000A900186D010088D0FA8D0200EAEAEA";
var offset = 0x8000;
var fr = new FileReader();
var romInput = document.getElementById('inputfile');
romInput.addEventListener('change', function () {
    var fr = new FileReader();
    //fr.readAsText(this.files[0]);
    //console.log(romInput.files);
    //console.log(fr);
    fr.onload = function () {
        //console.log(fr.result);
        cart.initCartridge(new Uint8Array(fr.result), bus); //pass the cartridge class the rom's binary data in UInt8 format
        //i am literally guessing these numbers
        cpu.pc = 0x8000;
        // bus.cpuRam[0xFFFC] = 0x00;
        // bus.cpuRam[0xFFFD] = 0x80;
        ////console.log(romInput);
    };
    fr.readAsArrayBuffer(romInput.files[0]);
});
// //this will likely need a lot of tweaking but might just work for now
// function loadRom(rom: string, offset: number) {
// 	for (let i=0; i<rom.length; i+=2){
// 		bus.cpuRam[offset++] = parseInt(rom.substring(i,i+2), 16);
// 	}
// 	//set reset vector
// 	bus.cpuRam[0xFFFC] = 0x00;
// 	bus.cpuRam[0xFFFD] = 0x80;
// 	cpu.reset();
// 	cpu.pc = 0x8000;
// }
// loadRom(rom, offset);
document.addEventListener("keydown", keyDownHandler, false);
var running;
function emLoop() {
    var start = Date.now();
    var lastUpdate = start;
    
    while (running) {
        console.log(Date.now() - lastUpdate);
        if (Date.now() - lastUpdate > 160) {
            lastUpdate = Date.now();
            cpu.cycles = 0;
            //cpu.clock();
            while (!ppu.frameComplete) {
                bus.clock();
            }
            ppu.frameComplete = false;
        }
    }
}
function keyDownHandler(e) {
    if (e.keyCode == 32) {
        cpu.cycles = 0;
        //cpu.clock();
        while (!ppu.frameComplete) {
            bus.clock();
        }
        ppu.frameComplete = false;
    }
    else if (e.keyCode == 82) {
        running = !running;
        emLoop();
    }
}
/* test program. compile at https://www.masswerk.at/6502/assembler.html
*=$8000
LDX #10
STX $0000
LDX #3
STX $0001
LDY $0000
LDA #0
CLC
loop
ADC $0001
DEY
BNE loop
STA $0002
NOP
NOP
NOP */ 
