package at.sabaini.emkuuclient.msg;

public class SubscribeMsg extends AbstractMessage {
	public String authorization;
	public String replyto;
	public String accept;
	public String cachecontrol;
	public String ifmod;
	public String lastmod;
	public boolean omitbody = false;
	public String subscribeuntil;
}
