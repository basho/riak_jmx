
all: jar

compile:
	@mvn compile

jar: compile
	@mvn package
	@mvn test
	@mv target/riak-jmx-*-jar-with-dependencies.jar ../priv/riak_jmx.jar

clean:
	@mvn clean
	@rm ../priv/riak_jmx.jar


