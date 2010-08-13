package at.sabaini.emkuuclient;

import at.sabaini.emkuuclient.msg.AbstractMessage;

public interface IMsgHandler {

	public void handle(AbstractMessage m);
}
