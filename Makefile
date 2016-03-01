all: .compiled

SOURCE = File.scala Match.scala

.compiled: $(SOURCE)
	@mkdir -p bin
	fsc -d bin $(SOURCE)
	touch $@