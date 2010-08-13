package at.sabaini.emkuuclient;

import at.sabaini.emkuuclient.msg.AbstractMessage;

public interface IEmkuuMsgCallback {

	public void call(AbstractMessage m);
}
