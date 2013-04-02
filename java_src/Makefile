
all: jar

compile:
	@mvn compile

jar: compile
	@mvn package
	@mv target/riak-jmx-1.0-SNAPSHOT-jar-with-dependencies.jar ../priv/riak_jmx.jar

clean:
	@mvn clean


