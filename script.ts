class mos6502 { //the cpu find test programs at https://codegolf.stackexchange.com/questions/12844/emulate-a-mos-6502-cpu
    bus: Bus;
    flags: Object;
    a: number;
    x: number;
    y: number;
    stkp: number;
    pc: number;
    status: number;
    fetched: number;
    absAddr: number;
    relAddr: number;
    opcode: number;
    cycles: number;
    lookup: Array<Array<string | number | {(): number}>>;

    constructor() {
        this.flags = { // a table to allow you to easily type flags 
			C : 1 << 0,	// Carry Bit
			Z : 1 << 1,	// Zero
			I : 1 << 2,	// Disable Interrupts
			D : 1 << 3,	// Decimal Mode (unused in this implementation)
			B : 1 << 4,	// Break
			U : 1 << 5,	// Unused
			V : 1 << 6,	// Overflow
			N : 1 << 7,	// Negative
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
        this.lookup = 	[
			[ "BRK", this.BRK, this.IMP, 7 ],[ "ORA", this.ORA, this.IZX, 6 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 3 ],[ "ORA", this.ORA, this.ZP0, 3 ],[ "ASL", this.ASL, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "PHP", this.PHP, this.IMP, 3 ],[ "ORA", this.ORA, this.IMM, 2 ],[ "ASL", this.ASL, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.NOP, this.IMP, 4 ],[ "ORA", this.ORA, this.ABS, 4 ],[ "ASL", this.ASL, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
			[ "BPL", this.BPL, this.REL, 2 ],[ "ORA", this.ORA, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 4 ],[ "ORA", this.ORA, this.ZPX, 4 ],[ "ASL", this.ASL, this.ZPX, 6 ],[ "???", this.XXX, this.IMP, 6 ],[ "CLC", this.CLC, this.IMP, 2 ],[ "ORA", this.ORA, this.ABY, 4 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 7 ],[ "???", this.NOP, this.IMP, 4 ],[ "ORA", this.ORA, this.ABX, 4 ],[ "ASL", this.ASL, this.ABX, 7 ],[ "???", this.XXX, this.IMP, 7 ],
			[ "JSR", this.JSR, this.ABS, 6 ],[ "AND", this.AND, this.IZX, 6 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "BIT", this.BIT, this.ZP0, 3 ],[ "AND", this.AND, this.ZP0, 3 ],[ "ROL", this.ROL, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "PLP", this.PLP, this.IMP, 4 ],[ "AND", this.AND, this.IMM, 2 ],[ "ROL", this.ROL, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "BIT", this.BIT, this.ABS, 4 ],[ "AND", this.AND, this.ABS, 4 ],[ "ROL", this.ROL, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
			[ "BMI", this.BMI, this.REL, 2 ],[ "AND", this.AND, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 4 ],[ "AND", this.AND, this.ZPX, 4 ],[ "ROL", this.ROL, this.ZPX, 6 ],[ "???", this.XXX, this.IMP, 6 ],[ "SEC", this.SEC, this.IMP, 2 ],[ "AND", this.AND, this.ABY, 4 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 7 ],[ "???", this.NOP, this.IMP, 4 ],[ "AND", this.AND, this.ABX, 4 ],[ "ROL", this.ROL, this.ABX, 7 ],[ "???", this.XXX, this.IMP, 7 ],
			[ "RTI", this.RTI, this.IMP, 6 ],[ "EOR", this.EOR, this.IZX, 6 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 3 ],[ "EOR", this.EOR, this.ZP0, 3 ],[ "LSR", this.LSR, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "PHA", this.PHA, this.IMP, 3 ],[ "EOR", this.EOR, this.IMM, 2 ],[ "LSR", this.LSR, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "JMP", this.JMP, this.ABS, 3 ],[ "EOR", this.EOR, this.ABS, 4 ],[ "LSR", this.LSR, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
			[ "BVC", this.BVC, this.REL, 2 ],[ "EOR", this.EOR, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 4 ],[ "EOR", this.EOR, this.ZPX, 4 ],[ "LSR", this.LSR, this.ZPX, 6 ],[ "???", this.XXX, this.IMP, 6 ],[ "CLI", this.CLI, this.IMP, 2 ],[ "EOR", this.EOR, this.ABY, 4 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 7 ],[ "???", this.NOP, this.IMP, 4 ],[ "EOR", this.EOR, this.ABX, 4 ],[ "LSR", this.LSR, this.ABX, 7 ],[ "???", this.XXX, this.IMP, 7 ],
			[ "RTS", this.RTS, this.IMP, 6 ],[ "ADC", this.ADC, this.IZX, 6 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 3 ],[ "ADC", this.ADC, this.ZP0, 3 ],[ "ROR", this.ROR, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "PLA", this.PLA, this.IMP, 4 ],[ "ADC", this.ADC, this.IMM, 2 ],[ "ROR", this.ROR, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "JMP", this.JMP, this.IND, 5 ],[ "ADC", this.ADC, this.ABS, 4 ],[ "ROR", this.ROR, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
			[ "BVS", this.BVS, this.REL, 2 ],[ "ADC", this.ADC, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 4 ],[ "ADC", this.ADC, this.ZPX, 4 ],[ "ROR", this.ROR, this.ZPX, 6 ],[ "???", this.XXX, this.IMP, 6 ],[ "SEI", this.SEI, this.IMP, 2 ],[ "ADC", this.ADC, this.ABY, 4 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 7 ],[ "???", this.NOP, this.IMP, 4 ],[ "ADC", this.ADC, this.ABX, 4 ],[ "ROR", this.ROR, this.ABX, 7 ],[ "???", this.XXX, this.IMP, 7 ],
			[ "???", this.NOP, this.IMP, 2 ],[ "STA", this.STA, this.IZX, 6 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 6 ],[ "STY", this.STY, this.ZP0, 3 ],[ "STA", this.STA, this.ZP0, 3 ],[ "STX", this.STX, this.ZP0, 3 ],[ "???", this.XXX, this.IMP, 3 ],[ "DEY", this.DEY, this.IMP, 2 ],[ "???", this.NOP, this.IMP, 2 ],[ "TXA", this.TXA, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "STY", this.STY, this.ABS, 4 ],[ "STA", this.STA, this.ABS, 4 ],[ "STX", this.STX, this.ABS, 4 ],[ "???", this.XXX, this.IMP, 4 ],
			[ "BCC", this.BCC, this.REL, 2 ],[ "STA", this.STA, this.IZY, 6 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 6 ],[ "STY", this.STY, this.ZPX, 4 ],[ "STA", this.STA, this.ZPX, 4 ],[ "STX", this.STX, this.ZPY, 4 ],[ "???", this.XXX, this.IMP, 4 ],[ "TYA", this.TYA, this.IMP, 2 ],[ "STA", this.STA, this.ABY, 5 ],[ "TXS", this.TXS, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 5 ],[ "???", this.NOP, this.IMP, 5 ],[ "STA", this.STA, this.ABX, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "???", this.XXX, this.IMP, 5 ],
			[ "LDY", this.LDY, this.IMM, 2 ],[ "LDA", this.LDA, this.IZX, 6 ],[ "LDX", this.LDX, this.IMM, 2 ],[ "???", this.XXX, this.IMP, 6 ],[ "LDY", this.LDY, this.ZP0, 3 ],[ "LDA", this.LDA, this.ZP0, 3 ],[ "LDX", this.LDX, this.ZP0, 3 ],[ "???", this.XXX, this.IMP, 3 ],[ "TAY", this.TAY, this.IMP, 2 ],[ "LDA", this.LDA, this.IMM, 2 ],[ "TAX", this.TAX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "LDY", this.LDY, this.ABS, 4 ],[ "LDA", this.LDA, this.ABS, 4 ],[ "LDX", this.LDX, this.ABS, 4 ],[ "???", this.XXX, this.IMP, 4 ],
			[ "BCS", this.BCS, this.REL, 2 ],[ "LDA", this.LDA, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 5 ],[ "LDY", this.LDY, this.ZPX, 4 ],[ "LDA", this.LDA, this.ZPX, 4 ],[ "LDX", this.LDX, this.ZPY, 4 ],[ "???", this.XXX, this.IMP, 4 ],[ "CLV", this.CLV, this.IMP, 2 ],[ "LDA", this.LDA, this.ABY, 4 ],[ "TSX", this.TSX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 4 ],[ "LDY", this.LDY, this.ABX, 4 ],[ "LDA", this.LDA, this.ABX, 4 ],[ "LDX", this.LDX, this.ABY, 4 ],[ "???", this.XXX, this.IMP, 4 ],
			[ "CPY", this.CPY, this.IMM, 2 ],[ "CMP", this.CMP, this.IZX, 6 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "CPY", this.CPY, this.ZP0, 3 ],[ "CMP", this.CMP, this.ZP0, 3 ],[ "DEC", this.DEC, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "INY", this.INY, this.IMP, 2 ],[ "CMP", this.CMP, this.IMM, 2 ],[ "DEX", this.DEX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "CPY", this.CPY, this.ABS, 4 ],[ "CMP", this.CMP, this.ABS, 4 ],[ "DEC", this.DEC, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
			[ "BNE", this.BNE, this.REL, 2 ],[ "CMP", this.CMP, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 4 ],[ "CMP", this.CMP, this.ZPX, 4 ],[ "DEC", this.DEC, this.ZPX, 6 ],[ "???", this.XXX, this.IMP, 6 ],[ "CLD", this.CLD, this.IMP, 2 ],[ "CMP", this.CMP, this.ABY, 4 ],[ "NOP", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 7 ],[ "???", this.NOP, this.IMP, 4 ],[ "CMP", this.CMP, this.ABX, 4 ],[ "DEC", this.DEC, this.ABX, 7 ],[ "???", this.XXX, this.IMP, 7 ],
			[ "CPX", this.CPX, this.IMM, 2 ],[ "SBC", this.SBC, this.IZX, 6 ],[ "???", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "CPX", this.CPX, this.ZP0, 3 ],[ "SBC", this.SBC, this.ZP0, 3 ],[ "INC", this.INC, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "INX", this.INX, this.IMP, 2 ],[ "SBC", this.SBC, this.IMM, 2 ],[ "NOP", this.NOP, this.IMP, 2 ],[ "???", this.SBC, this.IMP, 2 ],[ "CPX", this.CPX, this.ABS, 4 ],[ "SBC", this.SBC, this.ABS, 4 ],[ "INC", this.INC, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
			[ "BEQ", this.BEQ, this.REL, 2 ],[ "SBC", this.SBC, this.IZY, 5 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 4 ],[ "SBC", this.SBC, this.ZPX, 4 ],[ "INC", this.INC, this.ZPX, 6 ],[ "???", this.XXX, this.IMP, 6 ],[ "SED", this.SED, this.IMP, 2 ],[ "SBC", this.SBC, this.ABY, 4 ],[ "NOP", this.NOP, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 7 ],[ "???", this.NOP, this.IMP, 4 ],[ "SBC", this.SBC, this.ABX, 4 ],[ "INC", this.INC, this.ABX, 7 ],[ "???", this.XXX, this.IMP, 7 ],
		];
    }

    connectBus(bus: typeof Bus) {
        this.bus = bus;
    }

    write(addr: number, val: number) {
        this.bus.cpuWrite(addr, val);
    }

    read(addr: number) {
        return this.bus.cpuRead(addr);
    }

    getFlag(i: number) { //check the and of the flags
		return((this.status & i) ? 1 : 0);
	}

    setFlag(i, i2) { //set the i flag to i2
		if(i2){
			this.status |= i;
		}else{
			this.status &= ~i;
		}
	}

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
    reset() { //pass a reset signal
		this.absAddr = 0xFFFC;
		let lo = this.read(this.absAddr + 0);
		let hi = this.read(this.absAddr + 1);

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
    irq() { //pass an interupt request signal
		if(this.getFlag(this.flags.I) == 0) {
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
			let lo = this.read(this.absAddr + 0);
			let hi = this.read(this.absAddr + 1);
	
			this.pc = (hi << 8) | lo;

			this.cycles = 7; 
		}
	};
	nmi(){ //non-maskable interupt
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
		let lo = this.read(this.absAddr + 0);
		let hi = this.read(this.absAddr + 1);

		this.pc = (hi << 8) | lo;

		this.cycles = 8; 
	};

    clock() { //pass a clock cycle
		if(this.cycles == 0){//if its time for next opcode
            //read the current opcode and increment the programcounter
			this.opcode = this.read(this.pc); 
            this.pc++;

            let opc = this.lookup[this.opcode]; //stores the opcode we need
			console.log(opc[0]);
            this.cycles = opc[3] as number;

            let ac1 = opc[2].bind(this)(); // find out if both methods request additional cycles
			let ac2 = opc[1].bind(this)();

            this.cycles += (ac1 & ac2);

        };
        this.cycles--
    };

    //adressing modes
	IMP() { //Implied addressing, the address is in the functions code. This could also mean they would like to operate upon the accumulator so we fetch that 
		this.fetched = this.a;
		return 0; 
	}

    IMM() { //Operate on the next byte
		this.absAddr = this.pc++;
		return 0;
	}

    ZP0() { //access an adress on the zeroth page. this save an entire byte over absolute addressing
		this.absAddr = this.read(this.pc);
		this.pc++;
		this.absAddr &= 0x00ff;
		return 0;
	}

    ZPX() { //access zero page with offset based on x
		this.absAddr = this.read(this.pc) + this.x;
		this.pc++
		this.absAddr &= 0x00ff;
		return 0;
	}
    
    ZPY() { //access zero page with offset based on y
		this.absAddr = this.read(this.pc) + this.y;
		this.pc++
		this.absAddr &= 0x00ff;
		return 0;
	}

    ABS() { //directly access a 16 bit address, storing the address as low, high
		let lo = this.read(this.pc);
		this.pc++;
		let hi = this.read(this.pc);
		this.pc++
		this.absAddr = (hi << 8) | lo;
		return 0;
	}

    ABX() { //absolute address with the x register as an offset
		let lo = this.read(this.pc);
		this.pc++;
		let hi = this.read(this.pc);
		this.pc++
		this.absAddr = (hi << 8) | lo;
		this.absAddr += this.x;
		if ((this.absAddr & 0xFF00) != (hi << 8)){ // if we moved a page when adding x, we need to requst time
			return 1;
		}else{
			return 0;
		}
	}

    ABY() { //absolute address with the y register as an offset
		let lo = this.read(this.pc);
		this.pc++;
		let hi = this.read(this.pc);
		this.pc++
		this.absAddr = (hi << 8) | lo;
		this.absAddr += this.y;
		if ((this.absAddr & 0xFF00) != (hi << 8)){ // if we moved a page when adding x, we need to requst time
			return 1;
		}else{
			return 0;
		}
	}

    IND() { //indirect adressing, the address points to an address
		let lo = this.read(this.pc);
		this.pc++;
		let hi = this.read(this.pc);
		this.pc++
		let ptr = (hi << 8) | lo;
		if (lo == 0x00FF){ //simulate page boundary bug
			this.absAddr = (this.read(ptr&0xFF00) << 8) | this.read(ptr+0);
		}else{ //otherwise behave normally
			this.absAddr = (this.read(ptr+1) << 8) | this.read(ptr+0);
		}
		return 0
	}

    IZX() { //indirect addressing of zero with x offset  X Will add to t (t=ptr)
		let t = this.read(this.pc);//temp variable
		this.pc++;
		let lo = this.read((t+this.x) & 0x00FF);
		let hi = this.read((t+this.x+1) & 0x00FF);
		this.absAddr = (hi << 8) | lo;
		return 0
	}

    IZY() { //unlike the IZX, IZY will add the y register to the final address instead of T
		let t = this.read(this.pc);//temp variable
		this.pc++;
		let lo = this.read(t & 0x00FF);
		let hi = this.read((t+1) & 0x00FF);
		this.absAddr = (hi << 8) | lo;
		this.absAddr += this.y;

		if ((this.absAddr & 0xFF00) != (hi << 8)){
			return 1
		}else{
			return 0
		}
	}

    REL() { //relative, only applies to branching instructions
        this.relAddr = this.read(this.pc);
        this.pc++;
        if(this.relAddr & 0x80) { //checking if byte 7 is equal to 1. if it is we jump backwards instead of forwards
            this.relAddr -= 256;
        }
        return 0;
    }

    //instructions

    //helper function that will grab data and store it in the fetched variable
    fetch() {
        if(this.lookup[this.opcode][2] != this.IMP){ //lily ensure this works asap
            this.fetched = this.read(this.absAddr);
        }
        return this.fetched;
    } 

	ADC() { //add with carry in 
		this.fetch();
		let temp = this.a + this.fetched + this.getFlag(this.flags.C); //POTENTIAL BUG: carry bit might not be accurate here

		this.setFlag(this.flags.C, temp > 255);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0);
		this.setFlag(this.flags.N, temp & 0x80);
		//set the v flag based on signed overflow
		this.setFlag(this.flags.V, (~(this.a ^ this.fetched) & this.a ^ temp) & 0x0080);
		this.a = temp & 0x00FF;
		return 1;
	}

	SBC() { //subtract with carry in
		this.fetch();
		
		let value = this.fetched ^ 0x0FF;

		let temp = this.a + value + this.getFlag(this.flags.C);
		this.setFlag(this.flags.C, temp & 0xFF00);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0);
		this.setFlag(this.flags.N, temp & 0x80);
		this.a = temp & 0x00FF;
		return 1;
	}

    AND() { //logic bitwise and
        this.fetch();
        this.a = this.a & this.fetched;
        this.setFlag(this.flags.Z, this.a==0); //set zero flag if a is 0
        this.setFlag(this.flags.constructor, this.a & 0x80);
        return 1
    }

	// Instruction: Arithmetic Shift Left
	// Function:    A = C <- (A << 1) <- 0
	// Flags Out:   N, Z, C
	ASL() {
		this.fetch();
		let temp = this.fetched << 1;
		this.setFlag(this.flags.C, (temp & 0xFF00) > 0);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x00);
		this.setFlag(this.flags.N, (temp & 0x80));
		return 1;
	}

	// Instruction: Branch if Carry Clear
	// Function:    if(C == 0) pc = address 
	BCC() {
		if(this.getFlag(this.flags.C) == 0){
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

            if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

            this.pc = this.absAddr;
		}
		return 0;
	}

    BCS() { //Branch if carry set
        if (this.getFlag(this.flags.C) == 1) {
            this.cycles++;
            this.absAddr = this.pc + this.relAddr;

            if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

            this.pc = this.absAddr;
        }
        return 0;
    }

	BEQ() { //branch if equal
		if (this.getFlag(this.flags.Z) == 1){
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

			if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

			this.pc = this.absAddr;
		}
		return 0;
	}

	BIT() {
		this.fetch();
		let temp = this.a & this.fetched;
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x00);
		this.setFlag(this.flags.N, (1 << 7));
		this.setFlag(this.flags.V, (1 << 6));
		return 0;
	}

	// Instruction: Branch if Negative
// Function:    if(N == 1) pc = address
	BMI() {
		if(this.getFlag(this.flags.N) == 1) {
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

			if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

			this.pc = this.absAddr;
		}
		return 0;
	}

	// Instruction: Branch if Not Equal
	// Function:    if(Z == 0) pc = address
	BNE() {
		if(this.getFlag(this.flags.Z) == 0){
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

			if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

			this.pc = this.absAddr;
		}
		return 0;
	}

	// Instruction: Branch if Positive
	// Function:    if(N == 0) pc = address
	BPL() {
		if(this.getFlag(this.flags.N) == 0){
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

			if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

			this.pc = this.absAddr;
		}
		return 0;
	}

	// Instruction: Break
	// Function:    Program Sourced Interrupt
	BRK() {
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
	}

	// Instruction: Branch if Overflow Clear
	// Function:    if(V == 0) pc = address
	BVC() {
		if(this.getFlag(this.flags.V) == 0){
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

			if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

			this.pc = this.absAddr;
		}
		return 0;
	}

	// Instruction: Branch if Overflow Set
	// Function:    if(V == 1) pc = address
	BVS() {
		if(this.getFlag(this.flags.V) == 1){
			this.cycles++;
			this.absAddr = this.pc + this.relAddr;

			if((this.absAddr & 0xFF00) != (this.pc & 0xFF00)) {
                this.cycles++;
            }

			this.pc = this.absAddr;
		}
		return 0;
	}

	CLC() { //clear carry flag
        this.setFlag(this.flags.C, false);
        return 0;
    }

	CLD() { //clear decimal flag
        this.setFlag(this.flags.D, false);
        return 0;
    }

	// Instruction: Disable Interrupts / Clear Interrupt Flag
	// Function:    I = 0
	CLI() { 
		this.setFlag(this.flags.I, false);
		return 0;
	}

	// Instruction: Compare Accumulator
	// Function:    C <- A >= M      Z <- (A - M) == 0
	// Flags Out:   N, C, Z
	CMP() {
		this.fetch();
		let temp = this.a - this.fetched;
		this.setFlag(this.flags.C, this.a >= this.fetched);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		return 1;
	}

	//compare x register
	CPX() {
		this.fetch();
		let temp = this.x - this.fetched;
		this.setFlag(this.flags.C, this.x >= this.fetched);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		return 0;
	}

	//compare y register
	CPY() {
		this.fetch();
		let temp = this.y - this.fetched;
		this.setFlag(this.flags.C, this.y >= this.fetched);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		return 0;
	}

	// Instruction: Decrement Value at Memory Location
	// Function:    M = M - 1
	// Flags Out:   N, Z
	DEC() {
		this.fetch();
		let temp = this.fetched - 1;
		this.write(this.absAddr, temp & 0x00FF);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		return 0;
	}

	DEX() { //decrement x register
		this.x--;
		this.setFlag(this.flags.Z, this.x == 0x00);
		this.setFlag(this.flags.N, this.x & 0x80);
		return 0;
	}

	DEY() { // decrement y register
		this.y--;
		this.setFlag(this.flags.Z, this.y == 0x00);
		this.setFlag(this.flags.N, this.y & 0x80);
		return 0;
	}

	// Instruction: Bitwise Logic XOR
	// Function:    A = A xor M
	// Flags Out:   N, Z
	EOR() {
		this.fetch();
		this.a = this.a ^ this.fetched;
		this.setFlag(this.flags.Z, this.a == 0x00);
		this.setFlag(this.flags.N, this.a & 0x80);
		return 1;
	}

	// Instruction: Increment Value at Memory Location
	// Function:    M = M + 1
	// Flags Out:   N, Z
	INC() {
		this.fetch();
		let temp = this.fetched + 1;
		this.write(this.absAddr, temp & 0x00FF);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		return 0;
	}

	INX() { //increment x register
		this.x++;
		this.setFlag(this.flags.Z, this.x == 0x00);
		this.setFlag(this.flags.N, this.x & 0x80);
		return 0;
	}

	INY() { // increment y register
		this.y++;
		this.setFlag(this.flags.Z, this.y == 0x00);
		this.setFlag(this.flags.N, this.y & 0x80);
		return 0;
	}

	// Instruction: Jump To Location
	// Function:    pc = address
	JMP() {
		this.pc = this.absAddr;
		return 0;
	}

	// Instruction: Jump To Sub-Routine
	// Function:    Push current pc to stack, pc = address
	JSR() {
		this.pc--;
		this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF);
		this.stkp--;
		this.write(0x0100 + this.stkp, this.pc & 0x00FF);
		this.stkp--;

		this.pc = this.absAddr;
		return 0;
	}

	// Instruction: Load The Accumulator
	// Function:    A = M
	// Flags Out:   N, Z
	LDA() {
		this.fetch();
		this.a = this.fetched;
		this.setFlag(this.flags.Z, this.a == 0x00);
		this.setFlag(this.flags.N, this.a & 0x80);
		return 1;
	}

	LDX() { //load the x register
		this.fetch();
		this.x = this.fetched;
		this.setFlag(this.flags.Z, this.x == 0x00);
		this.setFlag(this.flags.N, this.x & 0x80);
		return 1;
	}

	LDY() { //load the y register
		this.fetch();
		this.y = this.fetched;
		this.setFlag(this.flags.Z, this.y == 0x00);
		this.setFlag(this.flags.N, this.y & 0x80);
		return 1;
	}

	LSR() {
		this.fetch();
		this.setFlag(this.flags.C, this.fetched & 0x0001);
		let temp = this.fetched >> 1;
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		if(this.lookup[this.opcode][2] == this.IMP){
			this.a = temp & 0x00FF;
		}else{
			this.write(this.absAddr, temp & 0x00FF);
		}
		return 0;
	}

	NOP() { // ILLEGAL OPCODES
		switch(this.opcode){} //TODO: Implement NOP timings
		return 0;
	}

	// Instruction: Bitwise Logic OR
	// Function:    A = A | M
	// Flags Out:   N, Z
	ORA() {
		this.fetch();
		this.a = this.a | this.fetched;
		this.setFlag(this.flags.Z, this.a == 0x00);
		this.setFlag(this.flags.N, this.a & 0x80);
		return 1;		
	}

	PHA() { //pushes accumulator to the stack
		this.write(0x0100 + this.stkp, this.a);
		this.stkp--;
		return 0;
	}

	// Instruction: Push Status Register to Stack
	// Function:    status -> stack
	// Note:        Break flag is set to 1 before push
	PHP() {
		this.write(0x0100 + this.stkp, this.status | this.flags.B);
		this.setFlag(this.flags.B, 0);
		this.setFlag(this.flags.U, 0);
		this.stkp--;
		return 0;
	}

	PLA() { //set the accumulator from the stackpointer
		this.stkp++;
		this.a = this.read(0x0100 + this.stkp);
		this.setFlag(this.flags.Z, this.a == 0x00);
		this.setFlag(this.flags.N, this.a & 0x80);
		return 0;
	}

	// Instruction: Pop Status Register off Stack
	// Function:    Status <- stack
	PLP() {
		this.stkp++;
		this.status = this.read(0x0100 + this.stkp);
		this.setFlag(this.flags.U, 1);
		return 0;
	}

	ROL() {
		this.fetch();
		let temp = (this.fetched << 1 | this.getFlag(this.flags.C));
		this.setFlag(this.flags.C, temp & 0xFF00);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x0000);
		this.setFlag(this.flags.N, temp & 0x0080);
		if(this.lookup[this.opcode][2] == this.IMP){
			this.a = temp & 0x00FF;
		}else {
			this.write(this.absAddr, temp & 0x00FF);
		}
		return 0;
	}

	ROR() {
		this.fetch();
		let temp = (this.fetched >> 1) | (this.getFlag(this.flags.C) << 7);
		this.setFlag(this.flags.C, this.fetched & 0x01);
		this.setFlag(this.flags.Z, (temp & 0x00FF) == 0x00);
		this.setFlag(this.flags.N, temp & 0x0080);
		if(this.lookup[this.opcode][2] == this.IMP){
			this.a = temp & 0x00FF;
		}else {
			this.write(this.absAddr, temp & 0x00FF);
		}
		return 0;
	}

	RTI() { //runs after an interupt
		this.stkp++;
		this.status = this.read(0x0100 + this.stkp);
		this.status &= ~this.getFlag(this.flags.B);
		this.status &= ~this.getFlag(this.flags.U);

		this.stkp++;
		this.pc = this.read(0x0100 + this.stkp);
		this.stkp++;
		this.pc |= this.read(0x0100 + this.stkp) << 8;
		return 0;
	}

	RTS() {
		this.stkp++;
		this.pc = this.read(0x0100 + this.stkp);
		this.stkp++;
		this.pc |= this.read(0x0100 + this.stkp) << 8;

		this.pc++;
		return 0;
	}

	SEC() { //set carry flag
		this.setFlag(this.flags.C, true);
		return 0;
	}

	SED() {
		this.setFlag(this.flags.D, true);
		return 0;
	}

	SEI() {
		this.setFlag(this.flags.I, true);
		return 0;
	}

	STA() {//store A in memory 
		this.write(this.absAddr, this.a);
		return 0;
	}

	STX() {//store A in memory 
		this.write(this.absAddr, this.x);
		return 0;
	}

	STY() {//store y in memory 
		this.write(this.absAddr, this.y);
		return 0;
	}

	// Instruction: Transfer Accumulator to X Register
	// Function:    X = A
	// Flags Out:   N, Z
	TAX() {
		this.x = this.a;
		this.setFlag(this.flags.Z, this.x == 0x00);
		this.setFlag(this.flags.N, this.x & 0x80);
		return 0;
	}
	
	TAY() {
		this.y = this.a;
		this.setFlag(this.flags.Z, this.y == 0x00);
		this.setFlag(this.flags.N, this.y & 0x80);
		return 0;
	}

	TSX() { //transfer stack pointer to x
		this.x = this.stkp;
		this.setFlag(this.flags.Z, this.x == 0x00);
		this.setFlag(this.flags.N, this.x & 0x80);
		return 0;
	}
	
	TXA() { //transfer x to accumulator
		this.a = this.x;
		this.setFlag(this.flags.Z, this.a == 0x00);
		this.setFlag(this.flags.N, this.a & 0x80);
		return 0;
	}
	
	TXS() { //transfer x to stack pointer 
		this.stkp = this.x;
		return 0;
	}
	
	TYA() { //transfer y to accumulator
		this.a = this.y;
		this.setFlag(this.flags.Z, this.a == 0x00);
		this.setFlag(this.flags.N, this.a & 0x80);
		return 0;
	}
	
	//capture all illegal opcodes
	XXX() {
		return 0;
	}
}

class PPU { //pixel procccessing unit. connects to the cpu bus but also has its smaller bus it can connect to 
	cartridge: Cartridge;
	nameTable: number[][]; //vram
	paletteTable: number[]; //stores pallete information
	constructor() {
		this.paletteTable = [0]; //stores 32 bytes
		this.nameTable = [[0]]; //one name table is 1kb. nes can store 2;
		for(let i=0;i<2;i++){
			this.nameTable[i] = [0];
			for(let j=0;j<1024;j++){
				this.nameTable[i][j] = 0;
			}
		}
	}

	cpuRead(addr: number){
		let data = 0x00;
		switch(addr){
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
		return data
	};
	cpuWrite(addr: number, val: number){
		switch(addr){
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
	ppuRead(addr: number){
		let data = 0x00;
		addr &= 0x3FFF;
		
		if(this.cartridge.ppuRead(addr)){

		}
		
		return data
	};
	ppuWrite(addr: number, val: number){;
		addr &= 0x3FFF;
		if(this.cartridge.ppuWrite(addr, val)){
			
		}
	}

	connectCartridge(cart: Cartridge) {
		this.cartridge = cart;
	}

	clock() {

	}

}

class Mapper { //super class for all mappers to be based off of 
	PRGBanks: number;
	CHRBanks: number;

	constructor(pBanks: number, cBanks: number) {
		this.PRGBanks = pBanks;
		this.CHRBanks = cBanks;
	}

	//virtual functions to be overwritten in extensions of this class
	cpuMapRead(addr: number){};
	cpuMapWrite(addr: number){};
	ppuMapRead(addr: number){};
	ppuMapWrite(addr: number){};
}

class Mapper_000 extends Mapper {
	constructor(pBanks: number, cBanks: number){
		super(pBanks, cBanks);
	}

	cpuMapRead(addr: number){
		if(addr >= 0x8000 && addr <= 0xFFFF){
			let mappedAddr = addr & (this.PRGBanks > 1 ? 0x7FFF : 0x3FFF);
			return mappedAddr;
		}
		return -1;
	};
	cpuMapWrite(addr: number){;
		if(addr >= 0x8000 && addr <= 0xFFFF){
			let mappedAddr = addr & (this.PRGBanks > 1 ? 0x7FFF : 0x3FFF);
			return mappedAddr;
		}
		return -1;
	}
	ppuMapRead(addr: number){		
		if(addr >= 0x0000 && addr <= 0x1FFF){
			let mappedAddr = addr;
			return mappedAddr;
		}
		return -1;
	};
	ppuMapWrite(addr: number){

		return -1;
	};
}

interface romHeader {
	name: string;
	prgRomChunks: number; //size of prg rom in 16kb units
	chrRomChunks: number; //size of chr rom in 8kb units
	mapper1: number;
	mapper2: number;
	prgRamSize: number;
	tvSystem1: number;
	tvSystem2: number;
	unused: string;
}

class Cartridge {
	PRGMem: number[]; //program memory for CPU
	CHRMem: number[]; //graphical data for PPU
	
	mapperID: number; //which mapper are we using?
	mapper: Mapper; //reference to mapper for this cartridge
	mirror: string;
	PRGBanks: number; //how many prg banks?
	CHRBanks: number; //how many chr banks?

	header: romHeader //header struct for nes roms (iNES format)

	constructor() {
		this.header = 	{
			name: "",
			prgRomChunks: 0,//size of prg rom in 16kb units
			chrRomChunks: 0, //size of chr rom in 8kb units
			mapper1: 0,
			mapper2: 0,
			prgRamSize: 0,
			tvSystem1: 0,
			tvSystem2: 0,
			unused: ""
		}
		this.PRGBanks = 0;
		this.PRGMem = [0];
		this.CHRBanks = 0;
		this.CHRMem = [0];
		this.mirror = "";//handles mirroring for the nametable which is responsible for displaying backgrounds
	}

	initCartridge(data: Uint8Array, bus: Bus) {
		console.log(this);
		bus.insertCartridge(this);
		let cartridgePntr = 16; //we will init header using a for loop but after that we will rely on this var to point to where we want to read from the cartride;
		//get header
		for(let i=0; i<16; i++){
			if(i<3){ //set name
				this.header.name += data[i].toString(16);
			}else if(i==4){
				this.header.prgRomChunks = data[i];
			}else if(i==5){
				this.header.chrRomChunks = data[i];
			}else if(i==6){
				this.header.mapper1 = data[i];
			}else if(i==7){
				this.header.mapper2 = data[i];
			}else if(i==8){
				this.header.prgRamSize = data[i];
			}else if(i==9){
				this.header.tvSystem1 = data[i];
			}else if(i==10){
				this.header.tvSystem2 = data[i];
			}else if(i>10){
				this.header.unused += data[i].toString(16);
			}
		}
		console.log(this.header);

		if(this.header.mapper1 & 0x04) { //checks if "trainer" exists. if it does we skip past it;
			cartridgePntr += 512; //Lily this may be an inanaccurate please check back if things dont work
		}

		this.mirror = this.header.mapper1 & 0x01 ? "vertical" : "horizontal";

		//get mapper id. mapper 2 stores the upper nybble and mapper 1 stores the lower nyble of the id
		this.mapperID = ((this.header.mapper2 >> 4) << 4) | (this.header.mapper1 >> 4);

		//get type of our iNES file. for now we will only deal with type1
		let nFileType = 1;

		if (nFileType == 0) {

		}

		if(nFileType == 1) {
			this.PRGBanks = this.header.prgRomChunks; //get size of prg memory and then read it in from the rom
			for(let i=0;i<this.PRGBanks*16384;i++){
				this.PRGMem[i] = data[cartridgePntr];
				cartridgePntr++;
			}
			this.CHRBanks = this.header.chrRomChunks; //get size of prg memory and then read it in from the rom
			for(let i=0;i<this.CHRBanks*8192;i++){
				this.CHRMem[i] = data[cartridgePntr];
				cartridgePntr++;
			}

			console.log(this);
		}

		if(nFileType == 2) {

		}

		switch(this.mapperID){
			case 0:
				this.mapper = new Mapper_000(this.header.prgRomChunks, this.header.chrRomChunks);
		}
	}

	//will hijack read/writes from the bus/ppu if the addr is within the cartridges range
	cpuRead(addr: number) {
		let mappedAddr = this.mapper.cpuMapRead(addr);
		let data = -1;
		if(mappedAddr > -1){
			data = this.PRGMem[mappedAddr];
		}
		console.log(data);
		return data;
	};
	cpuWrite(addr: number, val: number) {
		let mappedAddr = this.mapper.cpuMapRead(addr); 
		if (mappedAddr > -1) {
			this.PRGMem[mappedAddr] = val;
			return true;
		} else {
			return false;
		}
	};
	ppuRead(addr: number) {
		let mappedAddr = this.mapper.ppuMapRead(addr);
		let data = -1;
		if(mappedAddr > -1){
			data = this.CHRMem[mappedAddr];
		}
		return data;
	};
	ppuWrite(addr: number, val: number) {
		return false;
	};
}

class Bus {   //the bus. connects stuff
    cpuRam: number[];
	cpu: mos6502;
	cartridge: Cartridge;
	ppu: PPU;
	clockCounter: number;
    constructor() {
		this.cpuRam = [0];//2kb ram for the CPU
		this.clockCounter = 0; 
		for(let i=0;i<2048;i++){
			this.cpuRam[i] = 0;
		}
	}

    cpuWrite(addr: number, val: number) {
		if(this.cartridge.cpuWrite(addr, val)){
			//check if cpu write is in cartridge address range
		}else if(addr >= 0x0000 && addr <= 0x1FFF){
			this.cpuRam[addr & 0x07FF] = val;
		}else if(addr >= 0x2000 && addr <= 0x3FFF){
			this.ppu.cpuWrite(addr & 0x0007, val);
		}
    }

    cpuRead(addr: number) {
		let data = 0x00;
        if(this.cartridge.cpuRead(addr) > -1){
			//check if cpu read is in cartridge address range
			data = this.cartridge.cpuRead(addr);
			console.log("bus:", data);
		}else if(addr >= 0x0000 && addr <= 0x1FFF){
			data = this.cpuRam[addr & 0x07FF] // implements mirroring for the 8kb addressable range of the cpu ram 
		}else if(addr >= 0x2000 && addr <= 0x3FFF){
			data = this.ppu.cpuRead(addr & 0x0007);
		}
	
        return data;
    }

	//system functions
	insertCartridge(cart: Cartridge) {
		this.cartridge = cart;
		this.ppu.connectCartridge(cart);
	}

	connectPPU(ppu: PPU) {
		this.ppu = ppu;
	}

	reset() {
		this.cpu.reset();
		this.clockCounter = 0;
	}

	clock() {

	}
	
}

var cpu: mos6502 = new mos6502();
var ppu: PPU = new PPU();
var bus: Bus = new Bus();
var cart: Cartridge = new Cartridge();

cpu.connectBus(bus);
bus.connectPPU(ppu);

console.log(cpu.pc);
console.log(cpu.pc);
var rom: string = "A20A8E0000A2038E0100AC0000A900186D010088D0FA8D0200EAEAEA";
var offset: number = 0x8000;

var fr = new FileReader();
var romInput = document.getElementById('inputfile');
romInput.addEventListener('change', function () {
    var fr = new FileReader();
    //fr.readAsText(this.files[0]);
    console.log(romInput.files);
    console.log(fr);
    fr.onload = function () {
        console.log(fr.result);
		cart.initCartridge(new Uint8Array(fr.result as ArrayBuffer), bus); //pass the cartridge class the rom's binary data in UInt8 format
        //console.log(romInput);
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

var cpuDisplay = document.getElementById("cpuDisplay");

function renderCpuDisplay(display){
	console.log(display);
}

function keyDownHandler(e){
	if(e.keyCode == 32) {
		cpu.cycles = 0;
		cpu.clock();
		renderCpuDisplay(cpuDisplay);
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