#!/bin/bash
rm -rf leaf_spectra.db
sqlite3 leaf_spectra.db < schema.sql
