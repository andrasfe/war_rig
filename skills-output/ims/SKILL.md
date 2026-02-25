---
name: ims
description: IMS documentation (database/PSB definitions)
---

# IMS Documentation

| Name | Description | Documentation |
|---------|-------------|---------------|
| DBPAUTP0 | This is a Database Definition (DBD) source file for the IMS database DBPAUTP0, specifying a HIDAM database with VSAM access and no password. It defines one dataset group DSG001 using logical... | [Full docs](../documentation/ims/DBPAUTP0.dbd.md) |
| DBPAUTX0 | This DBD source defines the IMS database DBPAUTX0 as an index database using VSAM access with dataset DDPAUTX0. It specifies a single root segment PAUTINDX (6 bytes) with a unique packed decimal... | [Full docs](../documentation/ims/DBPAUTX0.dbd.md) |
| DLIGSAMP | This file is a Program Specification Block (PSB) source definition for the IMS DL/I application program DLIGSAMP, generated for COBOL language with no compatibility mode. It defines a database PCB... | [Full docs](../documentation/ims/DLIGSAMP.PSB.md) |
| PADFLDBD | This DBDGEN source file defines the IMS database PADFLDBD with dual access methods GSAM and BSAM, no password protection. It specifies Dataset Group 1 (DSG001) consisting of input dataset... | [Full docs](../documentation/ims/PADFLDBD.DBD.md) |
| PASFLDBD | This DBDGEN source file defines the IMS database PASFLDBD with GSAM and BSAM access methods and no password protection. It specifies a single dataset group DSG001 containing two fixed-block... | [Full docs](../documentation/ims/PASFLDBD.DBD.md) |
| PAUTBUNL | This file defines an IMS Program Specification Block (PSB) named PAUTBUNL for a COBOL program. It specifies a Database PCB (PAUTBPCB) for accessing the IMS database DBDNAME=DBPAUTP0 with... | [Full docs](../documentation/ims/PAUTBUNL.PSB.md) |
| PSBPAUTB | This file defines an IMS Program Specification Block (PSB) named PSBPAUTB for COBOL-language application programs. It specifies a single database PCB named PAUTBPCB that accesses the DBPAUTP0... | [Full docs](../documentation/ims/PSBPAUTB.psb.md) |
| PSBPAUTL | This file is an IMS Program Specification Block (PSB) generation control statement file that defines PSBPAUTL for assembler language programs. It specifies a single database PCB named PAUTLPCB for... | [Full docs](../documentation/ims/PSBPAUTL.psb.md) |
