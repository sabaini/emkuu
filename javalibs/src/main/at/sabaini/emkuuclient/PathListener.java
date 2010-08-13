package at.sabaini.emkuuclient;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import at.sabaini.emkuuclient.msg.AbstractMessage;

public abstract class PathListener implements IEmkuuMsgListener {

	private Pattern pat;
	
	public PathListener(String pathre) {
		this.pat = Pattern.compile(pathre);
	}
	public PathListener(String pathre, int flags) {
		this.pat = Pattern.compile(pathre, flags);
	}
	
	@Override
	public AbstractMessage handle(AbstractMessage msg) {
		String p = msg.to.clusterPath();
		Matcher m = this.pat.matcher(p);
		//System.out.println("PL: got " + msg + ", path " + p);
		if(m.lookingAt()) {
			//System.out.println("match");
			return this.handleMatch(msg, p.substring(m.end(), p.length()));
		}
		else
			return msg;
	}
	
	public abstract AbstractMessage handleMatch(AbstractMessage msg, String pathrest);

}
