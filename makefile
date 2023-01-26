all: main.jar

main.jar: bracket.scala database.scala elo.scala gui.scala
	scalac *.scala -d main.jar -deprecation -explain

run: main.jar
	scala main.jar

clean:
	rm main.jar