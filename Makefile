all: .compiled

SOURCE = MyTable.scala File.scala Match.scala 

.compiled: $(SOURCE)
	@mkdir -p bin
	fsc -d bin $(SOURCE)
	touch $@
