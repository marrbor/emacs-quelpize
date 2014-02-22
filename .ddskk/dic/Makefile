
DICURI:=http://openlab.ring.gr.jp/skk/skk/dic
DICPREFIX:=SKK-JISYO
DICS:=L JIS2 JIS3_4 notes assoc edict geo hukugougo jinmei law okinawa propernoun pubdic+ station

ZIPDICURI:=http://openlab.ring.gr.jp/skk/skk/dic/zipcode
ZIPDICS:=zipcode office.zipcode

DICFILES:=$(addprefix $(DICPREFIX).,$(DICS)) $(addprefix $(DICPREFIX).,$(ZIPDICS))

all: $(DICFILES)

$(DICPREFIX).zipcode:
	wget $(ZIPDICURI)/$@

$(DICPREFIX).%.zipcode:
	wget $(ZIPDICURI)/$@

$(DICPREFIX).%:
	wget $(DICURI)/$@

test:
	@echo "DICFILES:$(DICFILES)"







