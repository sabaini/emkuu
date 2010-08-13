package at.sabaini.emkuuclient;

public interface IEmkuuListenerFactory {

	public IEmkuuMsgListener create(
			EmkuuConnection con, 
			EmkuuManagedConnection mgr,
			Object[] args);
}
