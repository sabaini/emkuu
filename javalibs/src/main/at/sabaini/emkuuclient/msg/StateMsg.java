package at.sabaini.emkuuclient.msg;

public class StateMsg extends EntityMessage {
	public int status=200;
	public String etag;
	public String expires;
	public String location;
	public String locktimeout;
}
