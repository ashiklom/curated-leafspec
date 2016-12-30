library(specprocess)

projects <- fread('data/common/projects.csv')
mrg <- merge_with_sql(projects, 'projects', key = 'code')

specdb <- src_postgres('leaf_spectra')
tbl(specdb, 'projects')
