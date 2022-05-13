class olc6502 { //the cpu
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
		this.y = 0; //whore (general use) regist
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
			[ "BRK", this.BRK, this.IMM, 7 ],[ "ORA", this.ORA, this.IZX, 6 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 8 ],[ "???", this.NOP, this.IMP, 3 ],[ "ORA", this.ORA, this.ZP0, 3 ],[ "ASL", this.ASL, this.ZP0, 5 ],[ "???", this.XXX, this.IMP, 5 ],[ "PHP", this.PHP, this.IMP, 3 ],[ "ORA", this.ORA, this.IMM, 2 ],[ "ASL", this.ASL, this.IMP, 2 ],[ "???", this.XXX, this.IMP, 2 ],[ "???", this.NOP, this.IMP, 4 ],[ "ORA", this.ORA, this.ABS, 4 ],[ "ASL", this.ASL, this.ABS, 6 ],[ "???", this.XXX, this.IMP, 6 ],
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

    connectBus(bus: Bus) {
        this.bus = bus;
    }

    write(addr: number, val: number) {
        this.bus.write(addr, val);
    }

    read(addr: number) {
        return this.bus.read(addr);
    }

    getFlag(i: number) { //check the and of the flags
		return((this.status & i) ? 1 : 0);
	}

    setFlag(i, i2) { //set the i flag to i2
		if(i2){
			this.status |= i;
		}else{
			this.status &= ~i2;
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
    ADC(){};	AND(){};	ASL(){};	BCC(){};
	BCS(){};	BEQ(){};	BIT(){};	BMI(){};
	BNE(){};	BPL(){};	BRK(){};	BVC(){};
	BVS(){};	CLC(){};	CLD(){};	CLI(){};
	CLV(){};	CMP(){};	CPX(){};	CPY(){};
	DEC(){};	DEX(){};	DEY(){};	EOR(){};
	INC(){};	INX(){};	INY(){};	JMP(){};
	JSR(){};	LDA(){};	LDX(){};	LDY(){};
	LSR(){};	NOP(){};	ORA(){};	PHA(){};
	PHP(){};	PLA(){};	PLP(){};	ROL(){};
	ROR(){};	RTI(){};	RTS(){};	SBC(){};
	SEC(){};	SED(){};	SEI(){};	STA(){};
	STX(){};	STY(){};	TAX(){};	TAY(){};
	TSX(){};	TXA(){};	TXS(){};	TYA(){};
	XXX(){}; //this function will catch all unofficial opcodes and as a NOP call
    
    // these three functions are asynchronous. they are activated externally and will change the state of the cpu. 
    reset(){}; //pass a reset signal
    irq(){}; //pass an interupt request signal
    nmi(){}; //non-maskable interupt

    clock() { //pass a clock cycle
		if(this.cycles == 0){//if its time for next opcode
            //read the current opcode and increment the programcounter
			this.opcode = this.read(this.pc); 
            this.pc++;

            let opc = this.lookup[this.opcode]; //stores the opcode we need

            this.cycles = opc[3] as number;

            let ac1 = opc[2](); // find out if both methods request additional cycles
			let ac2 = opc[1]();

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
            this.relAddr |= 0xFF00;
        }
        return 0;
    }

    //instructions

    //helper function that will grab data and store it in the fetched variable
    fetch() {
        if(this.lookup[this.opcode] != this.IMP){
            this.fetched = this.read(this.absAddr);
        }
        return this.fetched;
    } 

    AND() { //logic bitwise and
        this.fetch();
        this.a = this.a & this.fetched;
        this.setFlag(this.flags.Z, this.a==0); //set zero flag if a is 0
        this.setFlag(this.flags.constructor, this.a & 0x80);
        return 1
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

    CLC() { //clear carry flag
        this.setFlag(this.flags.C, false);
        return 0;
    }

}

class Bus {   //the bus. connects stuff
    ram: number[];
    constructor() {
		this.ram = [0];//create a 64k test ram and set its contents to 0
		for(let i=0;i<64*1024;i++){
			this.ram[i] = 0;
		}
	}

    write(addr: number, val: number) {
        if(addr >= 0x0000 && addr <= 0xFFFF){
			this.ram[addr] = val;
		}
    }

    read(addr: number) {
        if(addr >= 0x0000 && addr <= 0xFFFF){
			return this.ram[addr];
		}
        return 0x00;
    }
}

var cpu: olc6502 = new olc6502();
var bus: Bus = new Bus();

cpu.connectBus(bus);