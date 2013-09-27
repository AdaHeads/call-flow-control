all: postgresql/schema.sql sqlite/schema.sql

%.sql: %.patch schema.sql
	cp -p schema.sql $@
	cd `dirname $@` && patch < schema.patch

clean:

distclean: clean
	rm -f */schema.sql

