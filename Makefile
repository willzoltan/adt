all: .compiled

SOURCE = MyTable.scala File.scala Machine.scala

.compiled: $(SOURCE)
	@mkdir -p bin
	fsc -d bin $(SOURCE)
	touch $@
