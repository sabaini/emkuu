package at.sabaini.test;

import at.sabaini.emkuuclient.IInitHandler;
import at.sabaini.emkuuclient.IMsgHandler;
import at.sabaini.emkuuclient.MsgId;
import at.sabaini.emkuuclient.msg.AbstractMessage;

public class TestMsgHandler implements IMsgHandler, IInitHandler {

	public AbstractMessage msg;

	public void handle(AbstractMessage m) {
		this.msg = m;
	}

	@Override
	public void handleInit(MsgId msgid) {
		// TODO Auto-generated method stub
		
	}
}
