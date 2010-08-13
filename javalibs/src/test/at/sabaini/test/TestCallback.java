package at.sabaini.test;

import at.sabaini.emkuuclient.IEmkuuMsgCallback;
import at.sabaini.emkuuclient.msg.AbstractMessage;

public class TestCallback implements IEmkuuMsgCallback {

	public AbstractMessage msg;

	@Override
	public void call(AbstractMessage m) {
		this.msg = m;
	}

}
