all: dict.d.byte myset.d.byte pagerank.d.byte moogle.d.byte

BUILD = ocamlbuild

ARGS = -libs unix,str

# These must be in the right order--no forward refs
DICT_FILES = order.ml dict.ml

SET_FILES = $(DICT_FILES) myset.ml

RANK_FILES = $(SET_FILES) graph.ml nodescore.ml util.ml query.ml pagerank.ml

MOOGLE_FILES = $(RANK_FILES) crawl.ml moogle.ml 

dict.d.byte: $(DICT_FILES)
	$(BUILD) $(ARGS) dict.d.byte

myset.d.byte: $(SET_FILES)
	$(BUILD) $(ARGS) myset.d.byte

pagerank.d.byte: $(RANK_FILES)
	$(BUILD) $(ARGS) pagerank.d.byte

moogle.d.byte: $(MOOGLE_FILES)
	$(BUILD) $(ARGS) moogle.d.byte

test: dict.d.byte myset.d.byte pagerank.d.byte
	./myset.d.byte
	./dict.d.byte
	./pagerank.d.byte 8080 200 wiki

serve: moogle.d.byte
	./moogle.d.byte 8080 200 wiki

clean: 
	ocamlbuild -clean 
