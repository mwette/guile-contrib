all: struct.info struct.text

struct.info: struct.texi wrap-struct.texi
	texi2any --info --no-split wrap-struct.texi

struct.text: struct.info
	cp struct.info struct.text
	sed -e '1,9d' -e '$-7,$-0d' struct.info > struct.text

clean:
	rm -f *.info
