
all: jar

compile:
	@mvn compile

jar: compile
	@mvn package
	@mv target/riak-jmx-1.4.0-jar-with-dependencies.jar ../priv/riak_jmx.jar

clean:
	@mvn clean


