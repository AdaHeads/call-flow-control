all: postgresql/schema.sql postgresql/test_data.sql sqlite/schema.sql sqlite/test_data.sql

%/schema.sql: %/schema.patch schema.sql
	cp -p schema.sql $@
	cd `dirname $@` && patch < schema.patch

%/test_data.sql: %/test_data.patch test_data.sql
	cp -p test_data.sql $@
	cd `dirname $@` && patch < test_data.patch

clean:

distclean: clean
	rm -f */{schema,test_data}.sql

