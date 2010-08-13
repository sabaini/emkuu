package at.sabaini.test;

import at.sabaini.emkuuclient.IEmkuuMsgListener;
import at.sabaini.emkuuclient.msg.AbstractMessage;

public class TestListener implements IEmkuuMsgListener {

	public AbstractMessage msg = null;

	public AbstractMessage handle(AbstractMessage m) {
		this.msg = m;
		return null;
	}
}
