package at.sabaini.emkuuclient;

import at.sabaini.emkuuclient.msg.AbstractMessage;

public interface IEmkuuMsgListener {

	EmkuuConnection connection = null;

	public AbstractMessage handle(AbstractMessage msg);
}
