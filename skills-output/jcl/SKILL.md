---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS program (CBPAUP0C) to delete expired authorizations. It defines the execution environment and necessary datasets for the IMS program to run as a BMP (Batch Message... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBPAUTP0 database using the IMS Database Image Copy utility (DFSRRC00). It first deletes the output dataset if it exists, then executes the unload, and finally catalogs... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL job executes an IMS program (DFSRRC00) to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program, PSB, and necessary datasets for the IMS... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes an IMS program (DFSRRC00) to unload a GSAM database. It specifies the program to execute, the IMS libraries required, and the input and output datasets. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads an IMS database (PAUTDB) and reloads it. It first deletes the existing database files, then executes the IMS program DFSRRC00 with the PAUDBUNL PSB to unload the database, and... | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
