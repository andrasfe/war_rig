---
name: ims
description: IMS documentation (database/PSB definitions)
---

# IMS Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| DBPAUTP0 | This file is the source code for the IMS Database Definition (DBD) named DBPAUTP0. It defines a HIDAM database using VSAM access with dataset DDPAUTP0 in dataset group 1. The structure includes... | [Full docs](../documentation/ims/DBPAUTP0.dbd.md) |
| DBPAUTX0 | Defines the IMS database DBPAUTX0 as an indexed, VSAM-protected database serving as an index pointer. Contains one dataset group DSG001 mapped to DDPAUTX0 with size 4096. Features a single root... | [Full docs](../documentation/ims/DBPAUTX0.dbd.md) |
| DLIGSAMP | This is the source file for generating an IMS Program Specification Block (PSB) named DLIGSAMP for a COBOL program (LANG=COBOL). It defines a primary database PCB (PAUTBPCB) for the IMS... | [Full docs](../documentation/ims/DLIGSAMP.PSB.md) |
| PADFLDBD | This is an IMS DBDGEN source file defining the PADFLDBD database with access methods GSAM and BSAM. It specifies dataset group DSG001 with input dataset DD1=PADFILIP and output dataset... | [Full docs](../documentation/ims/PADFLDBD.DBD.md) |
| PASFLDBD | This file is the DBDGEN source defining the IMS database PASFLDBD. It specifies GSAM and BSAM access methods with no password protection. A single dataset group DSG001 defines input dataset... | [Full docs](../documentation/ims/PASFLDBD.DBD.md) |
| PAUTBUNL | This file is the source specification for generating an IMS Program Specification Block (PSB) named PAUTBUNL for a COBOL language program. It defines a single database PCB named PAUTBPCB for... | [Full docs](../documentation/ims/PAUTBUNL.PSB.md) |
| PSBPAUTB | This file defines the IMS Program Specification Block (PSB) named PSBPAUTB for a COBOL application. It specifies a single Database PCB named PAUTBPCB accessing IMS database DBDNAME=DBPAUTP0 with... | [Full docs](../documentation/ims/PSBPAUTB.psb.md) |
| PSBPAUTL | This PSB defines a single database PCB named PAUTLPCB for accessing the IMS database DBDNAME=DBPAUTP0 with PROCOPT=L (enabling unqualified segment searches) and KEYLEN=14. It specifies two... | [Full docs](../documentation/ims/PSBPAUTL.psb.md) |
