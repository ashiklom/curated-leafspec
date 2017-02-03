library(specprocess)
specdb <- src_postgres('leaf_spectra')

projects <- read_csv('data/common/projects.csv')
mrg <- db_merge_into(db = specdb, table = 'projects', values = projects,
                     by = 'projectcode', id_colname = 'projectid')

tbl(specdb, 'projects')
