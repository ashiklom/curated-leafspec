library(specprocess)

projects <- fread('data/common/projects.csv')

specdb <- src_postgres('leaf_spectra')

mrg <- merge_with_sql(projects, 'projects', by = 'code')
