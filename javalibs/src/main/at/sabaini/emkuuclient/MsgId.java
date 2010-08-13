package at.sabaini.emkuuclient;

public class MsgId {

	public String txt;
	public long num;
	
	public MsgId(String s) {
		String[] s2 = s.split(":");
		this.txt = s2[0];
        this.num = Long.parseLong(s2[1]);
	}

	public String toString() {
		return this.txt + ":" + this.num;
	}
}
