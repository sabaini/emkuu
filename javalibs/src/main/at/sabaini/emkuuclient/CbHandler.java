package at.sabaini.emkuuclient;

import java.util.HashMap;
import java.util.Map;

import at.sabaini.emkuuclient.msg.AbstractMessage;

public class CbHandler implements IEmkuuMsgListener {

	public Map<Long, IEmkuuMsgCallback> cbMap = new HashMap<Long, IEmkuuMsgCallback>();
		
	@Override
	public AbstractMessage handle(AbstractMessage msg) {
		IEmkuuMsgCallback cb = this.cbMap.get(msg.corrid);
		//System.out.println("handle: " + msg + ", got cb: " + cb + ", map: " + this.cbMap);
		if(cb != null) {
			cb.call(msg);
			return null;
		} else 
			return msg;
	}

}
