all: main.jar

main.jar: main.scala
	scalac main.scala -d main.jar -deprecation -explain

run: main.jar
	scala main.jar

clean:
	rm main.jar