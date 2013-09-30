all: test.db

sources: postgresql/schema.sql postgresql/test_data.sql sqlite/schema.sql sqlite/test_data.sql

test.db: sqlite/schema.sql sqlite/test_data.sql
	rm -f test.db
	sqlite3 test.db < sqlite/schema.sql
	sqlite3 test.db < sqlite/test_data.sql

%/schema.sql: %/schema.patch schema.sql
	rm -f $@
	cp -p schema.sql $@
	cd `dirname $@` && patch < schema.patch

%/test_data.sql: %/test_data.patch test_data.sql
	rm -f $@
	cp -p test_data.sql $@
	cd `dirname $@` && patch < test_data.patch

clean:
	rm -f */schema.sql.orig
	rm -f */schema.sql.rej
	rm -f */test_data.sql.orig
	rm -f */test_data.sql.rej

distclean: clean
	rm -f test.db
	rm -f */schema.sql
	rm -f */test_data.sql

