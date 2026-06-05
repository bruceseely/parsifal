# notes/from-marcus/ — files received from Mitchell Marcus

Origin: `parse1.orig` (preserved as the canonical received artifact).
Source: Mitchell Marcus, sent 1987-11-22, received by the user 2026-06-03.

Each row below records one of the 11 file attachments concatenated in
`parse1.orig`. The Subject line of each enclosing email gave the
filename; mail-relay headers have been stripped from the extracted
files. The full original headers are reproduced after the table.

| Subject (filename) | Extracted to | Lines | Notes |
| --- | --- | --: | --- |
| `gram4.l` | `gram4.l` | 281 | the 1987 grammar rules |
| `util.l` | `util.l` | 790 |  |
| `load1.l` | `load1.l` | 18 |  |
| `pautil.l` | `pautil.l` | 193 |  |
| `~r patches.l` | `patches.l` | 113 |  |
| `glang.l` | `glang.l` | 616 | rule-language interpreter (Pratt-adapted parser) |
| `macros2.l` | `macros2.l` | 57 |  |
| `declr.l` | `declr.l` | 73 |  |
| `fixes.l` | `fixes.l` | 13 |  |
| `parsifal.help` | `parsifal.help` | 27 | plain-text help |
| `defs.l` | `defs.l` | 876 |  |

---

## Original mail-relay headers (one per attachment)

### gram4.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 15:49:56 1987
Posted-Date: Sun, 22 Nov 87 13:55 EST
Received-Date: Sun, 22 Nov 87 14:49:48 EST
Message-Id: <8711221949.AA01122@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 14:48 EDT
Received: from relay2.cs.net by RELAY.CS.NET id ac15244; 22 Nov 87 14:38 EST
Received: from btl by csnet-relay.csnet id ag13624; 22 Nov 87 14:34 EST
Date: Sun, 22 Nov 87 13:55 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: gram4.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### util.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:24:36 1987
Posted-Date: Sun, 22 Nov 87 13:56 EST
Received-Date: Sun, 22 Nov 87 18:24:20 EST
Message-Id: <8711222324.AA02589@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:22 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00810; 22 Nov 87 18:20 EST
Received: from btl by csnet-relay.csnet id aa14592; 22 Nov 87 18:13 EST
Date: Sun, 22 Nov 87 13:56 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: util.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### load1.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:31:04 1987
Posted-Date: Sun, 22 Nov 87 13:57 EST
Received-Date: Sun, 22 Nov 87 18:31:00 EST
Message-Id: <8711222331.AA02647@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:29 EDT
Received: from relay2.cs.net by RELAY.CS.NET id ac00851; 22 Nov 87 18:26 EST
Received: from btl by csnet-relay.csnet id ad14592; 22 Nov 87 18:21 EST
Date: Sun, 22 Nov 87 13:57 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: load1.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### pautil.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:31:06 1987
Posted-Date: Sun, 22 Nov 87 13:56 EST
Received-Date: Sun, 22 Nov 87 18:30:54 EST
Message-Id: <8711222330.AA02645@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:29 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00851; 22 Nov 87 18:26 EST
Received: from btl by csnet-relay.csnet id ab14592; 22 Nov 87 18:18 EST
Date: Sun, 22 Nov 87 13:56 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: pautil.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### patches.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:31:12 1987
Posted-Date: Sun, 22 Nov 87 13:56 EST
Received-Date: Sun, 22 Nov 87 18:31:02 EST
Message-Id: <8711222331.AA02653@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:29 EDT
Received: from relay2.cs.net by RELAY.CS.NET id ab00851; 22 Nov 87 18:26 EST
Received: from btl by csnet-relay.csnet id ac14592; 22 Nov 87 18:20 EST
Date: Sun, 22 Nov 87 13:56 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: ~r patches.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### glang.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:31:35 1987
Posted-Date: Sun, 22 Nov 87 13:57 EST
Received-Date: Sun, 22 Nov 87 18:31:23 EST
Message-Id: <8711222331.AA02666@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:29 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00893; 22 Nov 87 18:31 EST
Received: from btl by csnet-relay.csnet id ae14592; 22 Nov 87 18:21 EST
Date: Sun, 22 Nov 87 13:57 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: glang.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### macros2.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:38:57 1987
Posted-Date: Sun, 22 Nov 87 13:58 EST
Received-Date: Sun, 22 Nov 87 18:38:53 EST
Message-Id: <8711222338.AA02703@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:37 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00895; 22 Nov 87 18:31 EST
Received: from btl by csnet-relay.csnet id af14592; 22 Nov 87 18:26 EST
Date: Sun, 22 Nov 87 13:58 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: macros2.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### declr.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:39:10 1987
Posted-Date: Sun, 22 Nov 87 14:01 EST
Received-Date: Sun, 22 Nov 87 18:39:04 EST
Message-Id: <8711222339.AA02712@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:37 EDT
Received: from relay2.cs.net by RELAY.CS.NET id ab00895; 22 Nov 87 18:31 EST
Received: from btl by csnet-relay.csnet id ag14592; 22 Nov 87 18:26 EST
Date: Sun, 22 Nov 87 14:01 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: declr.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### fixes.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:39:13 1987
Posted-Date: Sun, 22 Nov 87 14:02 EST
Received-Date: Sun, 22 Nov 87 18:39:09 EST
Message-Id: <8711222339.AA02714@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:37 EDT
Received: from relay2.cs.net by RELAY.CS.NET id ac00895; 22 Nov 87 18:31 EST
Received: from btl by csnet-relay.csnet id ah14592; 22 Nov 87 18:27 EST
Date: Sun, 22 Nov 87 14:02 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: fixes.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### parsifal.help

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:41:18 1987
Posted-Date: Sun, 22 Nov 87 14:02 EST
Received-Date: Sun, 22 Nov 87 18:41:15 EST
Message-Id: <8711222341.AA02744@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:39 EDT
Received: from relay2.cs.net by RELAY.CS.NET id ab00922; 22 Nov 87 18:36 EST
Received: from btl by csnet-relay.csnet id aj14592; 22 Nov 87 18:32 EST
Date: Sun, 22 Nov 87 14:02 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: parsifal.help
To: research!mitch@linc.cis.upenn.edu
Status: R
```

### defs.l

```
From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:42:10 1987
Posted-Date: Sun, 22 Nov 87 14:01 EST
Received-Date: Sun, 22 Nov 87 18:41:53 EST
Message-Id: <8711222341.AA02753@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:39 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00924; 22 Nov 87 18:36 EST
Received: from btl by csnet-relay.csnet id ai14592; 22 Nov 87 18:27 EST
Date: Sun, 22 Nov 87 14:01 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: defs.l
To: research!mitch@linc.cis.upenn.edu
Status: R
```

