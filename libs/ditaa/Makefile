#
DITAA_VER:=0_9
DITAA_JAR:=ditaa$(DITAA_VER).jar

TARGET:=jditaa.jar

SRCS:=Jditaa.java
CLSS:=$(subst .java,.class,$(SRCS))

$(TARGET): $(DITAA_JAR) $(CLSS)
	cp $(DITAA_JAR) $@
	echo "Main-Class: Jditaa" >manifest
	jar umf manifest $@ $(CLSS)

%.class: %.java
	javac -cp $(DITAA_JAR) $<

clean:
	rm -rf $(CLSS) $(TARGET)

