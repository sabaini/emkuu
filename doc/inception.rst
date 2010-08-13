========================
 Designnotizen zu emkuu
========================

Stand: 2008-12-17 

1  Stichworte
2  Fragen
3  Konzepte
  3.1  Resourcen
  3.2  Adressierung
  3.3  Arten von Messages
    3.3.1  Commands
      3.3.1.1  Resource auslesen: get
      3.3.1.2  Resource aendern: put
      3.3.1.3  Resource anlegen: create / post
      3.3.1.4  Resource loeschen: delete
      3.3.1.5  Resource Commands: options
      3.3.1.6  Resource sperren: lock/unlock
      3.3.1.7  Resource-updates anfordern: subscribe / unsubscribe
      3.3.1.8  Resourcen-Weiterleitung: route
    3.3.2  State-Messages
  3.4  Messageformat
    3.4.1  Envelope
    3.4.2  Tag <cmd />
    3.4.3  Tag <state />
    3.4.4  Tag <fragment />
    3.4.5  Header-Daten
      3.4.5.1  Cmd - und State Messages
      3.4.5.2  Fragments
    3.4.6  Body
  3.5  Message Routing
    3.5.1  Netzwerkseitig
    3.5.2  Lokal
  3.6  Messagebroker
  3.7  Systemresourcen und Discovery
    3.7.1  Appregistry
      3.7.1.1  Filter
    3.7.2  Aktive Locks: /meta/locks
    3.7.3  Aktive Subscriptions: /meta/subscriptions
    3.7.4  Wartungstools und -Informationen
  3.8  Protokoll
  3.9  Architektur
4  Use Cases
  4.1  Application Resource Registration
    4.1.1  Goal
    4.1.2  Actors
    4.1.3  Preconditions
    4.1.4  Path
    4.1.5  Notes
  4.2  Function
    4.2.1  Goal
    4.2.2  Actors
    4.2.3  Preconditions
    4.2.4  Path
    4.2.5  Notes
  4.3  Update value
    4.3.1  Goal
    4.3.2  Actors
    4.3.3  Preconditions
    4.3.4  Path
    4.3.5  Notes
  4.4  Insert New Value
    4.4.1  Goal
    4.4.2  Actors
    4.4.3  Preconditions
    4.4.4  Path
    4.4.5  Notes
  4.5  Subscriptions
    4.5.1  Goal
    4.5.2  Actors
    4.5.3  Preconditions
    4.5.4  Path
    4.5.5  Notes
  4.6  Routing
    4.6.1  Goal
    4.6.2  Actors
    4.6.3  Preconditions
    4.6.4  Path
    4.6.5  Notes
  4.7  Worker Pool
    4.7.1  Goal
    4.7.2  Actors
    4.7.3  Preconditions
    4.7.4  Path
    4.7.5  Notes
5  Mediation
  5.1  Interbroker-Protokoll
    5.1.1  Interbroker ueber TCP/IP
  5.2  Application Protocol
6  Software Design
  6.1  Ablaeufe
  6.2  Packages und Module
    6.2.1   msgflow
    6.2.2   twistedtipc
    6.2.3   protocols.ibp
    6.2.4   protocols.ap
    6.2.5   storage.disk
    6.2.6   message
    6.2.7   net
    6.2.8   registry.applications
    6.2.9   registry.locks
    6.2.10  registry.subscriptions
  6.3  Datagram-Format


Stichworte
==========

0. RESTful Design

1. Dezentralisierte Broker

2. alle Broker halten Subscription Info

3. Subscription Info wird repliziert

4. Namensaufloesung von Brokern

5. Namen sind Transportunabhaengig (d.h. keine Ports etc.)

6. Messagepublisher und -consumer laufen in eigenen Prozessen,
   angekoppelt zB. ueber ein simples socket


Fragen
======

- Create kicken?

- Wie wuerden sich subscriptions verhalten bei vielen apps?
  State / Last am Server, leases, Tanenbaum: Content update
  propagation... 

  Antwort: testen
  
- Wie sollen sich subscriptions ueberhaupt verhalten? Subscribe zu einer
  eindeutigen Resource, einem Pattern, ...
  
  Antwort: das sollte in einer spaeteren Ausbaustufe angegangen
  werden, und ist ohnehin nur bei autonotify Apps moeglich 

- Kann man die E.I. Patterns in emkuu abbilden? Wie?
  
- Auf welche Arten kann man ausser xmpp/socket emkuu noch ansprechen?
  TCP? HTTP? SMTP? Soll es ein in-process Protokoll geben, f. plugins
  oae.? 
  
- Macht eine JS-Clientlib Sinn? Zb. basierend auf JSJaC oder xmpp4js?
  Dann muesste auch BOSH supported werden
  
- Welche System-Resourcen sollten die Broker exportieren? 
  
  Antwort: Appregistry, Subscriptions, Locks, Testapp; das sollte
  unter einer URL /meta zur Verfuegung stehen und wird grossteils
  read-only sein; auf der anderen Seite sollten zB. Subscriptions
  von /meta durchaus moeglich sein.
  
- Welche System-Management Supportfunktionen sollen vorgesehen werden?
  Ist eine emkuu-Shell machbar bzw. was waeren die use cases?

- Kann das emkuu broker Netzwerk als Applikation gesehen werden, in
  dem die Broker Messages ueber Client-Applikationen austauschen
  bzw. Resourcen ueber registrierte Client-Applikationen
  bereitstellen?

- Wie performant ist twisted.xish im Vergleich zu lxml, speziell
  XPath-Operationen? 

- In welche Module soll emkuu aufgeteilt werden? 

  Antwort: 

  Routing, Appregistry, Persistenz, XMPP-Handling, TIPC-Handling,
  Tests, Metadata, XMPP-basierendes Protokoll, TIPC-basierendes
  Protokoll

- Muessen Applikations-Teilbaeume nicht-ueberlappend sein? 

  Antwort: nein, s. overlappingresources unten; overlapping kann fuer
  bestimmte Applikationen Sinn machen (Replikation oae.)

- Welche Konfigurationsmoeglichkeiten sind fuer Applikationen
  vorzusehen? 

  Antwort:

  * appname: Namen der Applikation
  * appid: Numerische ID der Applikation(?autogen?)
  * socket: Unix- oa. Socket, ueber das die Applikation angebunden ist 
  * autonotify: Repr senden wenn eine Datenveraendernde Message auf
    die Subscriptions vorhanden sind gesendet wird  
  * autounsubscribe: Soll der Broker Unsubscribe-Messages senden bei
    Timeout? 
  * autounlock: Soll der Broker Unlock-Messages senden bei Timeout? 
  * assemblemsg: Sollen Fragmente zusammengefuehrt werden?
  * overlappingresources: Sollen ueberlappende Resourcenpfade fuer
    diese Applikation erlaubt werden?
  * Messagefilters: XPath oae. Kriterium, das eingehende und
    ausgehende Messages einer Applikation filtert. Auf diese Weise
    koennen zB. cmd-subscribe Messages von einer Applikation die diese
    nicht handeln will ferngehalten werden. 

- Wie ist das Fehlerverhalten von emkuu? Wie werden
  Applikationsfehler, Netzwerkfehler oder Brokerfehler behandelt? Wie
  greift das ins System-Management? Was ist mit Retourmessages? 

- Wie ist die Semantik von from: Adressen? Regeln noetig?
  Konventionen? 

- Welchen Library-Support brauchen Applikationen? Prepackaged
  XPath-Objekte? Protokollhandling? Sockethandling? ... 

- Welche Uhr gilt im emkuu-Netzwerk? Brauchen wir eine Uhr (Lamport,
  Vektor oae.)?

- Idee: fuer Transformationen von Content-Types koennten Filterplugins
  vorgesehen werden. Eine Applikation koennte f. bestimmte
  URI-Patterns bevorzugte Types registrieren, und ein Filter koennte
  Transformationen zwischen Types anbieten. Der Broker wuerde dann die
  jew. passende Transformation vornehmen. Filterplugins koennten als
  Pythonpackage oder als XSL-File implementiert werden. 

- Dauer von Subscriptions/Locks -- was passiert wenn Applikationen
  haeufig neu gestartet neu gestartet werden muessen und sich jedesmal
  (mit langen/unendlichen Timeouts) subskribieren oder locken?
  Garbage-Collection? Erkennung von doppelten subs/locks anhand des
  Absenders und der betreffenden Resource? Persistenz von
  Subskriptionen/Locks? Kann eine Applikation sich ihre alten
  Subs/Locks holen?

- Content-Type und State-Messages: falls eine State-Message nicht als
  Antwort auf ein cmd-get oder cmd-subscribe gesendet wird, woher soll
  die Senderapplikation wissen, welchen Content-Type sie senden soll?
  Evtl. nur mit omit-body? 

- To-Header bei State-Messages: eigentlich sollte sich der Adressat
  aus der corrid ergeben, wird der to: header gebraucht?



Konzepte
========

Resourcen
---------

Resourcen sind im Sinn von HTTP referenzierbare Daten. Resourcen
werden meist von Applikationen bereitgestellt. Ausgenommen davon sind
nur die Systeminformationen selbst, die von emkuu ebenfalls zur
Verfuegung gestellt werden. 

Resourcen sind anders als bei HTTP nicht nur Senken von Kommandos,
sondern auch Quellen (from / to Header). 

Zu jeder Resource gehoert eine In-Queue, die den State der Resource
beeinflusst, und eine Out-Queue, die Messages die von der Resource
gesendet werden empfaengt.

Resourcen werden von Commands referenziert, veraendert, geloescht,
bekanntgegeben etc. 


Adressierung
------------

Adressierung von Resourcen erfolgt ueber URI-artige Resource
identifier im xmpp:// Schema.

Jede Resource ist ein Topic; eine Queue ist ein Topic mit nur einem
Subscriber.

Adressen sollten grob folgendem Schema gehorchen:

  xmpp://${cluster}/${app}/${path}#${fragment}?${key1=val1;...}

cluster: Ein cluster bezeichnet alle Nodes die sich direkt, ohne Router, erreichen
koennen

app: ist eine App-Identifier

path: ist ein Pfad zu einer Resource

fragment: ein bestimmtes Teilfragment

key1=val1: bezeichnet 0 oder mehrere Query-Parameter

Hosts oder IP-Adressen sind nicht Teil des Schemas; Adressierung
erfolgt nur indirekt!


Arten von Messages
------------------

Es gibt, vom Standpunkt einer Applikation aus gesehen, zwei Arten
Messages: 

- Messages, die auf Resourcen von Peers wirken; diese werden als
  Commands bezeichnet
- Messages, die eine eigene Resource senden, die hier als
  State-Messages bezeichnet werden. State-Messages koennen, muessen
  aber nicht, als Antwort auf ein Command gesendet werden. 

Commands haben einen Empfaenger und einen Absender; State-Messages
muessen keinen Empfaenger haben (dieser kann nach Aufloesen der
Subscriptions vom Broker eingesetzt werden).

Commands
~~~~~~~~

Commands sind Aktionen, die Resourcen veraendern, abfragen, verteilen
etc.; sprich sie alle Adressieren eine Resource.

Einige, aber nicht alle, Commands sind von HTTP uebernommen
worden und verhalten sich aehnlich wie diese. 

Resource auslesen: get
++++++++++++++++++++++

Die get-Message liefert eine Repraesentation einer Resource
zurueck. Welche Form der Repraesentation wird durch den accept-Header
bestimmt, s.u. Verhalten aehnlich wie in HTTP.

Resource aendern: put
+++++++++++++++++++++

Die put-Message ueberschreibt den Wert der referenzierten Resource mit
der im Body mitgelieferten Entity. Verhalten aehnlich wie in HTTP.

Resource anlegen: create / post
+++++++++++++++++++++++++++++++

Die create/post-Commands erstellen eine neue Resource. create ersucht
den Peer zu einer gegebenen Resource-ID eine gueltige Subresourcen-ID
zurueckzuliefern. Diese kann anschliessend mit post einmal befuellt
werden; mehrmaliger Aufruf resultiert in einen Fehler ("command not
allowed").

Eine generierte Subresourcen-ID bedeutet lediglich, das der Peer ein post
auf diese ID erlaubt, nicht notwendigerweise dass nur die anfordernde
Applikation post ausfuehren darf. 

Siehe unten auch unter post-* header.


Resource loeschen: delete
+++++++++++++++++++++++++

Die unter der angegebenen ID liegende Resource soll geloescht
werden. Verhalten aehnlich wie in HTTP.

Resource Commands: options
++++++++++++++++++++++++++

Liefert die fuer diese Resource (und Header, zB. Authorization)
erlaubten Commands. Verhalten aehnlich wie in HTTP; ausser dass kein
eigenes Headerfeld, sondern schlicht der Body in der State die
Antwortdaten enthaelt. 


Resource sperren: lock/unlock
+++++++++++++++++++++++++++++

Die angegebene Resource wird gesperrt, und zwar fuer die im
lock-commands Header angegebenen Commands. Optional kann ein
gewuenschter Timeout-Wert mitgeliefert werden (lock-until).

Der Peer identifiziert locks anhand des Tupels (Absender,
Resource-ID); doppelte cmd-lock Commands bewirken nur eine Ruecksetzung
der Timer (Lock refresh).

Der Peer liefert ein lock-timeout zurueck, das vom geforderten
lock-until unterschiedlich sein kann.

Ein bestehendes Lock kann mit cmd-unlock geloescht werden.

Der emkuu-Broker sollte bestehende Locks auffindbar machen, zB. unter
der URL /meta/locks


Resource-updates anfordern: subscribe / unsubscribe
+++++++++++++++++++++++++++++++++++++++++++++++++++

Der Peer (bzw. der emkuu-Broker) wird beauftragt, Aenderungen der
angegebenen Resource der Applikation zuzusenden (bzw. nur die Header,
falls omit-body gesendet wird). 

Der Peer bzw. der emkuu-Broker identifizert eine Subscription anhand
des Tupels (Absender, Resource-ID). 

Optional kann die Applikation im Header subscribe-until die Dauer der
Subscription angeben. 

Der emkuu-Broker sollte die bestehenden Subscriptions auffindbar
machen, zB. unter /meta/subscriptions 


Resourcen-Weiterleitung: route
++++++++++++++++++++++++++++++

Der Peer wird beauftragt, eine Message weiterzuleiten. Dieses Command 
uebernimmt als Entity eine gekapselte Message (eig. Content-Type
"application/x-emkuu-msg"), die weitergeleitet werden soll. 

Das Ziel der Weiterleitung ist von Kriterien abhaengig, die
Applikationsspezifisch sind. Moeglichkeiten waeren zB.:

- Zieladresse ist die to-Adresse in der gekapselten Message
- Zieladresse ist abhaengig von Headern oder Entity der gekapselten Message
- Zieladresse ist abhaengig von externen Kriterien (Zeit, Zufall,
  vorhergehende oder nachfolgende Messages) 
- eine Kombination aus den obigen 

Jedenfalls sollte der routende Peer die to-Adresse in der gekapselten
Message so umschreiben, dass sie mit der effektiven Adresse
uebereinstimmt. 


State-Messages
~~~~~~~~~~~~~~

Im Unterschied zu Commands haben State-Messages nicht den Zweck, auf
einen definierten Peer bzw. dessen Resource einzuwirken. Stattdessen 
uebermitteln lediglich den State einer bestimmten Resource, bzw. die
Notifikation, dass sich dieselbe geaendert hat (omit-body Header).


Messageuniversum
----------------

Das Messageuniversum ist die Gesamtheit aller derzeit in emkuu
bekannten Messages. Das sind einerseits die derzeit gerade
angenommenen, oder sich in Zustellung befindlichen Messages, inklusive
der momentan nicht zustellbaren gequeuten Messages. Andererseits
werden Metadaten zu Messages aber auch eine gewisse Zeitspanne zwecks
Duplikaterkennung aufbewahrt; diese Messages gehoeren ebenfalls zum
Messageuniversum. 


Messageformat
-------------

Envelope
~~~~~~~~

Messages sind in XMPP Message stanzas eingebettet: <message />. Darin
sind entweder <cmd />, <state /> oder <fragment /> Tags
geschachtelt. 

Tag <cmd />
~~~~~~~~~~~

Tags <cmd /> stellen eine Aufforderung dar, ein Command
auszufuehren. Dementsprechend haben sie auch ein Attribut do="" das
das gewuenschte Command angibt.

Tag <state />
~~~~~~~~~~~~~

Diese Tags senden eine Repraesentation einer internen Resource. Das
kann in Antwort auf ein Command geschehen, oder auch ohne externen
Ausloeser. 

Tag <fragment />
~~~~~~~~~~~~~~~~

Messages koennen in mehrere Fragmente aufgeteilt werden. Motivation
dazu kann einerseits die Groesse sein (beim Transport ueber TIPC/AMP
muss das ab einer Groesse von 64kB geschehen), andererseits weiss eine
Applikation moeglicherweise nicht, wie gross die Message insgesamt
werden wird (Streaming).

Bei der Fragmentierung wird eine regulaere Cmd/State-Message
gesendet, mit einem fragid-Headerfeld. Die weiteren Fragmente haben
dann einen reduzierten Satz an Headern, naemlich die fragid und einen
lastfrag-Marker. 


Header-Daten
~~~~~~~~~~~~

Alle Arten von Messages weisen eine Headersektion auf.

Cmd - und State Messages
++++++++++++++++++++++++++

Vordefinierte Felder (Muss-Felder sind mit * gekennzeichnet):

fro *
  Cmd- und State. Der Absender der Message; eine URI

to * 
  Cmd- und State. Ein oder mehrere Adressaten; URIs

msgid
  Cmd- und State. Eine Message-ID; int
  Das Tupel {fro, msgid} ist fuer das Messageuniversum eindeutig
  (dzt. Implementierung: msgid ist eindeutig).

  msgid werden von emkuu Brokern generiert; Applikationen sollten
  keine msgids vergeben.

corrid
  Cmd- und State. Messages notieren hier die ID des Requests dessen
  Antwort sie sind, wie "In-Reply-To". Bei Cmd-Messages: Vorgabe fuer
  die corrid der erwarteten State. int

reply-to 
  Cmd-Messages geben hier die Adresse, an die die Antwort gesendet
  werden soll, falls diese von der from-Adresse verschieden ist; URI
  
status * 
  State. Status der Anfrage; sind in Anlehnung an HTTP definiert;
  int 

return-path (?)
  Proxies / Forwarder tragen hier ihre URI ein
  
accept 
  Cmd. Wie bei HTTP: welche Form der Representation in der Antwort
  gewuenscht wird; ein MIME-Typ. 

  Accept-Charset hat uebrigens kein Aequivalent in emkuu; Charset
  sollte grundsaetzlich UTF-8 sein 

authorization
  Cmd: Enthaelt die anzuwendenden Authorisierungsinformationen

cache-control
  Cmd, State. Wie bei HTTP.

content-length
  Cmd, State. Anzahl Bytes des Entitybodies

content-type *
  State, Cmds die Entities uebermitteln (put, post): MIME-Type des
  Entitybodies 

date
  Cmd, State. Timestamp nach RFC 3339. Applikationen duerfen Messages
  ohne Timestamp versenden; dieser sollte von emkuu eingefuegt werden.

etag
  Lamport-Timestamp . Appid

if-modified-since
last-modified
if-none-match
expires

  Diese sind wie in HTTP definiert zu verwenden. 

location
  Wenn eine Resource eine neue URI hat, oder gerade erst erstellt wird
  oder wurde

fragment
  Cmd und States. Wenn vorhanden, zeigt dies an, dass der
  Messagebody in mehrere Teile aufgeteilt ist. Siehe <fragment /> -
  Tag.

omit-body 
  Cmd oder State. Wenn gesetzt, soll der Body einer State
  weggelassen werden.

post-until
  State. Antwort-Header auf cmd-create, der der aufrufenden
  Applikation eine Gueltigkeitsdauer der generierten Resource-ID
  gibt. 

post-token
  State auf cmd-create oder cmd-post. Liefert der aufrufenden
  Applikation ein Token, unter der der post entgegengenommen
  wird. Dieses Token ist transient, dh. nur fuer einen post-request
  gueltig, und unique.

lock-commands
  Cmd-lock. Die Applikation teilt dem Peer hier die Commands mit, die
  gelockt werden sollen. Wenn nicht vorhanden, wird als default put
  angenommen. 
  
lock-until
  Cmd-lock. Hier kann die Applikation dem Peer eine gewuenschte
  Lockdauer mitteilen. 

lock-timeout
  State auf cmd-lock. Zeit, die der Peer ein Lock bestehen laesst.

subscribe-until
  Cmd-subscribe. Hier kann die Applikation dem Peer bzw. dem Broker die
  gewuenschte Dauer der Subscription mitteilen.
  


Fragments
+++++++++

Headerfelder einer Fragment-Message sind gegenueber der zugehoerigen
Cmd/State-Message stark reduziert. 

ID *
  Bezeichnet die Message, zu der ein Fragment gehoert
  
fragid *
  Fortlaufende ID des Fragments, um das es sich handelt

lastfrag 
  Marker: wenn vorhanden, ist dieses Fragment das letzte. 
  


Body
~~~~

Messages haben eine optionale <body /> - Sektion, die Nutzdaten fuer
die Applikation transportiert. 

Der Body kann, muss aber nicht, XML enthalten. Wenn dem so ist, sollte
ein eigener default-Namespace verwendet werden.

Vor allem bei fragmentierten Messages kann es jedoch notwendig sein,
den Body-Content in eine CDATA-Sektion einzuwickeln.


Message Routing
---------------


Netzwerkseitig
~~~~~~~~~~~~~~

Es werden hauptsaechlich vom Netzwerk eingehende, an lokale
Apps gerichtete Messages behandelt. Ausnahmen sind die
Route-Commands, die eine Weiterleitungsanfrage darstellen, und die
Subscribe- und Lockcommands, die die Art des Routings selbst
veraendern, und nicht an lokale Apps weitergeleitet werden.

Subscribe-Anfragen werden vom Subscription Manager
behandelt und entsprechend gespeichert; aehnlich werden Lock-Anfragen
behandelt, sie werden ebenfalls an einen dedizierten Manager
weitergeleitet.

States und Get-, Head-Anfragen werden an die jew. Empfaenger
weitergeleitet, ebenso wie Put / Post / Delete -Anfragen.


Lokal
~~~~~

Von lokalen Apps eingehende Messages werden wieder unterschieden
nach Commandsaufrufen (<cmd />) und States (<state />).

Commandsaufrufe werden aehnlich behandelt, als ob sie von der
Netzwerkseite kaemen.

Bei Statemessages wird vor allem der Adressat behandelt. Eine
State hat iAllg. eine bestehende Empfaengeradresse. Wird im
Subscription Manager eine subskribierte Adresse gefunden, wird die zu
den Empfaengern hinzugefuegt.


Messagebroker
-------------

Netzwerkseitig: hoert auf ankommende Nachrichten, multiplext sie auf
angebundene Apps. Multiplexen bedeutet, Adressen aufzuloesen die
Message in die entsprechenden lokalen Queues zu stellen::

 (network-msg) --> (mb) --> (adr-reso) -->| discard non-local, log
                                |
          			+--> (local-queue1) --> (app1)
				|
       				+--> (local-queue2) --> (app2)


Lokal: nimmt Nachrichten von Apps entgegen, loest Adressen auf,
verteilt Nachrichten an Remote-Messagebroker::

 (apps) --> (mb) --> (adr-reso)
			 |
			 +--> (remote-mb1)
			 |
			 +--> (remote-mb2)


Systemresourcen und Discovery
-----------------------------

Das emkuu Brokernetzwerk verwaltet einige Systemresourcen, die unter
der URI /meta zugaenglich gemacht werden. Applikationen koennen sich
dort anmelden, Resource discovery betreiben und Wartungsinformationen
beziehen. 

Appregistry: /meta/processregistry
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In der Appregistry werden Metainformationen zu jeder Applikation
zusammengefasst. Das betrifft die Resourcen, die von der Applikation
zur Verfuegung gestellt werden, und verschiedene
Konfigurationseinstellungen. 

Ein Feature, das optional von den Apps angefordert werden kann, ist
autonotify. Unter der Annahme, dass die Resourcen einer App nur ueber
den Messagebroker veraendert werden, kann dieser auch, da er alle
Updates kennt, Events zu bestehenden Subscriptions dieser Resourcen
generieren.

Die Appregistry ist ueber die Systemurl /meta/applications
zugaenglich und kann sowohl gelesen als auch beschrieben werden. 


Health polling
++++++++++++++

Idee: emkuu koennte unter /meta/processregistry detaillierte
Informationen zum Status des Prozesses zur Verfuegung stellen,
zB. alive/not alive, uptime, load in msg/sec, oae.


Filter
++++++

Eine Applikation kann fuer sich Filter registrieren; diese koennen
sowohl auf eingehende als auch auf ausgehende Messages wirken. 

Filter koennen Messages, die eine Applikation nicht verarbeiten kann
fernhalten (und damit Bandbreite sparen), und situativ auch Messages
vom System fernhalten, die von imperfekten Applikationen gesendet
werden. 


Aktive Locks: /meta/locks
~~~~~~~~~~~~~~~~~~~~~~~~~

Unter dieser Resource sind alle momentan im System aktiven Locks
verzeichnet. Applikationen koennen nach gelockten Resourcen suchen, um
(look before you leap) herauszufinden ob ein Lockversuch ueberhaupt
erfolgreich sein wuerde.

xxx Commands zum cleanup?


Aktive Subscriptions: /meta/subscriptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unter dieser Resource sind alle momentan im System aktiven
Subscriptions verzeichnet. 

xxx Use cases?

Wartungstools und -Informationen
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

xxx
/meta/test 
/meta/deadletter
etc.



Protokoll
---------

Das emkuu-Protokoll ist eine XMPP-Extension und stark
HTTP-inspiriert.

Architektur
-----------

::

     +----------+
     | Netzwerk	|
     +----------+
       	  |
      	  |
      	  |
     +--------------+  	+---------------+
     | EmkuuService |---| NetworkRouter |
     +--------------+  	+---------------+
       	       	             \|
      		       	      |
       	       	       	      |
     +--------------+         |
     | Local Router |------+-- ------------+
     +--------------+      |  |		   |
       /|      	       	  /|  |        	  /|
       	|      	       	   |  |            |
       	|      +----------- --+------------ ----+
       	|     /	       	   |  |	       	   |   	|
     	|   |/ 	       	   |  |/       	   |   	|/
     +------------+    +------------+    +------------+
     | LocalProto |    | LocalProto |    | LocalProto |
     +------------+    +------------+    +------------+
            |                 |	       	       	|
     +-------------+   +-------------+   +-------------+
     | Local App 1 |   | Local App 2 |   | Local App N |
     +-------------+   +-------------+   +-------------+

xxx obsolet


Use Cases
=========

Application Resource Registration
---------------------------------

Goal
~~~~

Eine Applikation gibt dem Broker bekannt, welche Resourcenpfade
von der Applikation zur Verfuegung gestellt werden. 


Actors
~~~~~~

Applikation A, Broker B

Preconditions
~~~~~~~~~~~~~

A ist beim Broker B konfiguriert, d.h. grundsaetzliche Kommunikation
ist bereits hergestellt, und B's policy erlaubt Registrierung von A

Path
~~~~

- A sendet create/post Messages Mx an B an /meta/applications
- Mx enthalten die Resourcenpfade die A registrieren moechte
- B prueft, ob die betreffenden Resourceneintraege sich mit
  bestehenden ueberschneiden wuerden und wenn ja, ob das per
  overlappingresources erlaubt ist
- Wenn ja: B legt die betreffenden Eintraege unter /meta/applications
  an 

Notes
~~~~~

Welche Form die Resourcenpfade haben ist noch festzulegen -- exakte
Pfade waeren unsinnig; Teilbaeume oder Patterns wuerden sich
anbieten. Wobei bei Patterns schwierig zu entscheiden ist ob sich
Baeume ueberlappen(?)


Function
--------

Goal
~~~~

Eine Applikation benoetigt eine Berechnung 

Actors
~~~~~~

Applikation A, Berechnungskomponente B

Preconditions
~~~~~~~~~~~~~

Die Berechnung haengt nur von den Eingabedaten und hat keine
Seiteneffekte oder State. 

Path
~~~~

A sendet eine "get" Message an B, bei der alle noetigen Parameter im
URL-String kodiert sind. B fuehrt die Berechnung durch, und sendet das
Ergebnis zurueck.

Notes
~~~~~

- URL-Strings sind in emkuu nicht laengenbegrenzt, bzw. duerfen "sehr
  gross" werden
- B kann wenn sinnvoll die state natuerlich cachen


Update value
------------

Goal
~~~~

Eine Applikation moechte einen Wert einer Resource die zu einer
anderen Applikation gehoert, speichern

Actors
~~~~~~

Applikationen A, B, Resource R

Preconditions
~~~~~~~~~~~~~

R gehoert zu B, A kennt URI von R

Path
~~~~

A sendet eine put-Message mit einer geeigneten Represantation von R an
B . Wenn B updates nicht erlaubt, wird ein Errorcode 405 Command Not
Allowed zurueckgeliefert. Andernfalls wird mit 200 zurueckgeliefert


Notes
~~~~~

keine


Insert New Value
----------------

Goal
~~~~

Eine Applikation moechte eine neue Resource die zu einer anderen
Applikation gehoert, speichern

Actors
~~~~~~

Applikationen A, B, Resourcen C (Container), R (neue Resource), Rd
(Daten fuer neue Resource)

Preconditions
~~~~~~~~~~~~~

C, R gehoeren zu B, A kennt URI von C

Path
~~~~

- A sendet cmd-create an C. Optional kann A eine URI fuer die neue
  Resource im location-Header vorschlagen.
- B antwortet mit Status 200 und liefert im Body eine URI fuer die
  neue Resource R.
  Optional kann im Statebody eine Form mit den erwarteten
  Inputparametern zurueckgeliefert werden
- A sendet Daten Rd in einer "post" Message an R 
- B speichert Rd, retourniert 200
- Falls ein Fehler beim Speichern oder beim Versenden der Antwort
  auftritt, kann A das "post" wiederholen
- Falls A das "post" wiederholt, aber Rd schon gespeichert wurde, muss
  B mit 405 antworten


Notes
~~~~~

- Aehnlich wie "Post Once Exactly", draft-nottingham-http-poe-00 
- Die Antwort auf cmd-create kann, wenn die Applikation dies
  unterstuetzt, Header enthalten die entweder ein Zeitlimit fuer das
  cmd-post oder ein Token enthalten, die den cmd-post
  identifizieren. Dies ist aber, um den Aufwand fuer einfache
  Applikationen gering zu halten, optional.
- Librarysupport: fuer Client und Peer noetig


Subscriptions
-------------

Goal
~~~~

Eine Applikation moechte immer den aktuellen Stand einer
Resource kennen

Actors
~~~~~~

Applikation S (Subscriber), Applikation P (Publisher), Resource R,
Emkuu-Broker ES und EP

Preconditions
~~~~~~~~~~~~~

P muss State halten koennen: eine veraenderliche Resource, und
uU. Subscriptions 

Path
~~~~

S sendet eine Subscribe-Message an P (ueber ES und EP) fuer die
Resource R, mit einer definierten Leasezeit. Sofern die S in der
Konfig angegeben hat, dass ein unsubscribe gewuenscht wird, extrahiert
EP die Leasezeit; sofern S autonotify gesetzt hat, wird jedenfalls der
Subscriptionrequest gespeichert.

In jedem Fall wird ES einen TIPC-Socket, der auf den Multicast-Namen
von P hoert, aufmachen. 

Ohne autonotify: P sendet bei jeder Aenderung von R eine
Representation von R an S. Mit autonotify: EP sendet, sobald eine
Aenderung von R vorgenommen wird (mittels einer Message, die ueber EP
geroutet wird) eine Representation von R an S.

Die Message wird auf den Multicast-Namen von P ausgesendet. 

Ohne subrelease: Nach Ablauf der Lease hoert P auf, Aenderungen an R
zu versenden.

Mit subrelease: Nach Ablauf der Lease sendet EP eine
unsubscribe-Message an P.

Bei Eintreffen einer unsubscribe-Message von S wird die Subskription
ebenfalls geloescht bzw. keine geaenderten Representationen
versendet. 

Notes
~~~~~

Das Subscription-Feature bedeutet, dass Applikationen State ueber
Subskribienten halten muessen: Die Adresse und die Leasezeit. Um den
Aufwand gering zu halten sind fuer Resourcen, die Datenaenderungen nur
ueber den Broker erhalten, die autonotify und uU. subrelease
Konfigoptionen gedacht. 

Eventuell Alternative: Applikation schickt Updates an Broker, nur
dieser verwaltet Subscriptions. Da das bedingt das JEDES Update an den
Broker geschickt wird, geht das nur bei geringem Datenvolumen

Routing
-------

Goal
~~~~

Eine Nachricht Mb von einer Applikation A sollen abhaengig von
bestimmten Kriterien an unterschiedliche Empfaenger Bx weitergeleitet
werden, ueber eine Routingapplikation R; diese Bx sollen ihrerseits
wieder direkt an A antworten

Actors
~~~~~~

Applikationen A, Bx; Router R; Messages Ma, Mb

Preconditions
~~~~~~~~~~~~~

- Router R muss als Applikation fuer Routing eingerichtet sein
- Message Ma enthaelt Mb

Path
~~~~

- A sendet "cmd-route" Nachricht Ma mit ID a an R; Ma enthaelt Nachricht
  Mb (mit ID b).
- R prueft applikationsspezifische Parameter (als URI-Parameter
  uebergebene Werte, Content-Type, andere Header, Message Body, etc. )
  und ermittelt daraus Empfaenger Bx
- R sendet Nachricht Mb (ID b) an Bx; Mb hat den Absender A
- Bx bearbeitet Message, sendet State / Error an A


Notes
~~~~~

- Neuer Content-Type "application/x-emkuu-msg" sollte auf die Entity
  der route-Commands gesetzt werden. 


Worker Pool
-----------

Goal
~~~~

Eine Applikation vergibt Auftraege, die von einem oder mehreren
Workern abgearbeitet werden.

Actors
~~~~~~

Applikation A, Worker(s) Wx, Emkuu-Broker E

Preconditions
~~~~~~~~~~~~~

xxx

Path
~~~~

- A stellt eine Resource TL zur Verfuegung, die als Container fuer
  Tasks Tx fungiert. TL stellt eine Liste dieser Tx zur Verfuegung. 
- Alle Wx subskribieren TL
- Wenn A einen neuen Task Ty in Auftrag geben moechte, sendet A ein
  Update von TL, mit einem Link auf den neuen Eintrag Ty
- E versendet neues TL an Wx
- Alle aktiven Wx parsen TL und versuchen, Ty zu locken
- Der schnellste Wx sei Wa. Wa lockt Ty.
- Allen anderen Wx gelingt Lock-Versuch nicht
- Wa holt Ty. Ty enthaelt eine Taskbeschreibung.
- Wa arbeitet Task ab
- Wa loescht Ty 

Notes
~~~~~

- es ist unklar, ob Wa neben dem loeschen von Ty auch noch explizit
  unlocken muss
- Zur Vermeidung von Races beim konkurrierendem Lock sollte evtl. eine
  Holdoff-Periode eingefuehrt werden?
- Was passiert wenn Wa verstirbt? Kurze Lockzeiten und Re-locks? 

Error cases
-----------
xxx missing
wie gehen receiver mit fehlerhaften state messages um? cmd messages
ziehen eine <state> meldung nach sich. state?
invalid letter channel?
EIP, chap4 p20


Mediation
=========

Hier soll beschrieben werden, wie die Kommunikation im Netzwerk von
Brokern und Applikationen vonstatten geht. 

Interbroker-Protokoll
---------------------

Das Interbroker-Protokoll ibp dient der effizienten und sicheren
Kommunikation von emkuu-Brokern. Als Basis dient TIPC in Connection-
und Connectionless-Modus. Messageaustausch passiert auf mehreren Ports
in einem Binaerformat.

Connections werden verwendet, wenn ein einzelner anderer Knoten
angesprochen werden soll; ansonsten wird der Connectionless-Modus
verwendet.

Die Adressierung im Interbroker-Protokoll sieht die Verwendung von
TIPC-Namen vor. Um Applikationen unterscheiden zu koennen, wird jeder
Applikation eine numerische ID zugeordnet (appid). Analog werden
Brokern ebenfalls IDs zugeordnet (brokerid). 


Es soll folgendes Schema zur Namensgebung verwendet werden: 

- Broker Broadcast, {127, brokerid}: Jede Brokerinstanz hoert auf
  diesen Namen. Dieser Port dient dem Austausch von Systemnachrichten,
  zB. ueber die Praesenz von angeschlossenen Applikationen. Wenn eine
  Brokernachricht versendet werden soll, wird diese an
  {128,2,MAXBROKER} versendet; MAXBROKER ist zu definieren, wird aber
  <=4096 sein.

- Applikations-Multicast: Alle Applikationen x, die sich fuer Messages
  einer Applikation y mit der ID y_id interessieren, hoeren auf Port
  {y_id, x_id}, bzw. oeffnet der zustaendige Broker einen solchen Port,
  typischerweise beim Empfang einer "subscribe"-Message.

  Wenn Applikation y eine Message publishen moechte, sendet es diese
  an {y_id, 2, MAXAPPS}, mit MAXAPPS ~< 32000.
  
- Applikations-Streaming: Applikationen hoeren hier fuer Connections
  zur Uebertragung von Streams, und zwar auf den Port {appid, 1}


Datagram-Format
~~~~~~~~~~~~~~~

Message-Datagramme sind grundsaetzlich so aufgebaut:

  +-----------------------------------------------------------------+
  | CLS | fieldlen1 | field1 | fieldlen2 | field2 | fieldlenX | ... |
  +-----------------------------------------------------------------+
  | b   | 16bit     | len(1) | 16bit     | len(2) | ............... |
  +-----------------------------------------------------------------+
								   

D.h. zuerst einen Typidentifier CLS, ein Byte; danach Felder mit
vorangestellten Feldlaengen. Die Feldlaengen sind jeweils ein unsigned
short; die Felder so lange wie die Feldlaengen davor angeben. 

Der Typidentifier CLS gibt an um welche Art Message es sich handelt,
und damit auch, wie die restlichen Felder zu interpretieren sind. Es
gibt fuer die Messagetypen eine statische Tabelle, zB.:

  +----+-----------------+
  |CLS | Messageart      |
  +----+-----------------+
  |  1 | State           |
  |  2 | cmd get         |
  |  3 | cmd put         |
  |  4 | cmd delete      |
  |  5 | cmd create      |
  |  6 | cmd post        |
  |  7 | cmd options     |
  |  8 | cmd subscribe   |
  |  9 | cmd unsubscribe |
  | 10 | cmd lock        |
  | 11 | cmd unlock      |
  | 12 | cmd route       |
  | 13 | fragmentmsg     |
  +----+-----------------+



Interbroker ueber TCP/IP
~~~~~~~~~~~~~~~~~~~~~~~~

Ausblick: wie koennte das Interbroker-Protokoll ueber TCP/IP betrieben
werden? Zweckmaessigerweise ebenfalls AMP-basierend. Problem dabei ist
natuerlich das (sichere) Multicasting -- evtl. eine Art 2PC?

"ibptcp"


Application Protocol
--------------------

Das Application Protocol emkuu-ap ist eine Abbildung des oben
skizzierten Messageformats nach XML, und wird ueber XMPP
versendet. Basis ist dabei meist eine lokale Socket-Verbindung, da die
Applikationen typischerweise an einem lokalen Broker haengen; TCP/IP
sollte aber ebenso unterstuetzt werden.


Software Design
===============

Hier soll das High-Level Software Design festgelegt werden. Dazu
werden einzelne Aufgabenbereiche definiert, und einige Schnittstellen
dazu. 

Ablaeufe
--------

Ablaeufe innerhalb eines Brokers, nicht notwendigerweise in dieser
Reihenfolge: 

- Message wird von Applikation oder Broker empfangen

- Message wird persistent gemacht

- Message wird vorverarbeitet 
  * Parsen
  * fehlende Felder ergaenzen

- Subskribienten werden ermittelt und Message entsprechend intern
  adressiert 

  * Falls publish- oder subscribe-State

  * Falls write-Message und autonotify fuer diese Applikation
    aktiviert

- Namensaufloesung der Empfaenger, einsetzen Empfaengeradressen 
  * Messages fuer lokale Applikationen
  * Messages fuer remote Applikationen
  * Adminmessages

- cmd-subscribe Messages behandeln: zugehoerige Empfaenger speichern,
  weiterleiten

- cmd-lock Messages behandeln: wenn Applikation autounlock wuenscht,
  lock in Registry eintragen
  
- Filtern von local-in Messages

- Filtern von local-out Messages

- Versenden der Messages

  * wieder lokale Applikationen, remote Applikationen (via Broker),
    Adminmessages 



Packages und Module
-------------------

msgflow
~~~~~~~

Das msgflow - Modul ist fuer die Ablaufsteuerung verwantwortlich. Es
bekommt eingehende Messages zur Verfuegung gestellt, und verarbeitet
die Messages.

Messagedaten werden dazu in Task-Objekte gewrappt. Diese halten neben
den Messages auch die (Zwischen-) Ergebnisse der einzelnen
Verarbeitunsschritte. 

Es wird fuer jeden Messagetyp ein eigener Workflow in Form einer FSM
implementiert. Diese Workflows sind ueber die Konfigoptionen
einstellbar. Da die Workflows fuer unterschiedliche Messagetypen recht
unterschiedlich sein werden, macht es keinen Sinn sie in ein
FSM-Schema zu pressen. 

Die Verarbeitung wird ueber MessageProcessor - Objekte, die als Z3 -
Adapter implementiert sind, durchgefuehrt (Command-Pattern). 

Die einzelnen Komponenten des msgflow - Modules werden also sein:

* Task
  - Messages
  - States
  - Ergebnisse: outqueue, status, ...
* MessageProcessor
  - Input: Task
  - Output: Defered
* MessageFlow:
  - liest Konfiguration
  - steuert FSM


twistedtipc
~~~~~~~~~~~

TIPC-Support fuer Twisted

protocols.ibp
~~~~~~~~~~~~~

Implementiert das Interbroker-Protokoll. Basierend auf
AMP. 

protocols.ap
~~~~~~~~~~~~

Implementiert das Application Protocol. Hauptsaechlich
twisted.words.xish-basierende Klassen.

storage.disk
~~~~~~~~~~~~

Implementiert Persistenzmechanismen fuer Messages. 

message
~~~~~~~

Modelliert Messagearten

net
~~~

Das net Package nimmt eingehende Messages entgegen und versendet ausgehende
Messages. Dazu verwaltet es eine Reihe von Protocol-Instanzen. 


registry.applications
~~~~~~~~~~~~~~~~~~~~~

Applikationsdaten und Registry

registry.locks
~~~~~~~~~~~~~~

Registriert die vorhandenen Locks.

registry.subscriptions
~~~~~~~~~~~~~~~~~~~~~~

Registriert die vorhandenen Subscriptions.





