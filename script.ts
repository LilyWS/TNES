class Bus {
    ram: number[];
    constructor() {
		this.ram = [0];//create a 64k test ram and set its contents to 0
		for(let i=0;i<64*1024;i++){
			this.ram[i] = 0;
		}
	}
}