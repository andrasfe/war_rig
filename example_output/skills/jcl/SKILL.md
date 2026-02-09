---
name: jcl
description: JCL job documentation
---

# JCL Documentation

| Job | Description | Documentation |
|---------|-------------|---------------|
| CBPAUP0J | This JCL executes an IMS program (CBPAUP0C) to delete expired authorizations. It defines the execution environment, including program name, parameters, libraries, and input/output datasets... | [Full docs](../documentation/jcl/CBPAUP0J.jcl.md) |
| DBPAUTP0 | This JCL job unloads the DBD DBPAUTP0 to a sequential dataset. It first deletes the output dataset if it exists, then executes the IMS program DFSRRC00 with parameters to perform the unload. The... | [Full docs](../documentation/jcl/DBPAUTP0.jcl.md) |
| LOADPADB | This JCL executes the IMS program DFSRRC00 to load the PAUTDB database using a BMP (Batch Message Processing) region. It specifies the program DFSRRC00, the BMP region, the load utility PAUDBLOD,... | [Full docs](../documentation/jcl/LOADPADB.JCL.md) |
| UNLDGSAM | This JCL job executes the IMS program DFSRRC00 to unload a GSAM database. It specifies the IMS region, database libraries, and input/output datasets required for the unload process. | [Full docs](../documentation/jcl/UNLDGSAM.JCL.md) |
| UNLDPADB | This JCL job unloads an IMS database (PAUTDB) and reloads it. It first deletes the existing database files, then executes the IMS program DFSRRC00 with the PAUDBUNL PSB to unload the database, and... | [Full docs](../documentation/jcl/UNLDPADB.JCL.md) |
