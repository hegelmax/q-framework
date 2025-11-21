# Q-Framework for Dynamic SQL in MS SQL Server

Q-Framework is a lightweight but powerful dynamic SQL engine implemented entirely in T‑SQL.  
It provides a unified system for building SQL queries, injecting variables, managing versions, and generating dynamic SQL code safely and consistently.

## 🚀 Key Features

### 1. **Schema Initialization**
- Automatically creates required schemas: `q` and `_tmp_dynamic`
- Assigns permissions for public use
- Includes optional full cleanup of schema objects

### 2. **Versioning Engine**
- `q.sys_ver` — extracts version from object comments  
- `q.sys_drop` — safely drops outdated objects  
- `q.sys_upd_tbl_ver` — supports table versioning (`table_v1`, `table_v2`, …) and automatically regenerates synonyms  

### 3. **Conversion Functions**
Includes utilities for:
- Win1251 ↔ UTF‑8
- Binary ↔ Base64
- Integer ↔ Binary String
- Date ↔ Unix Time
- Date formatting by mask

### 4. **String Parsing Tools**
- Advanced string split (`q._split`, `q._split_row`)
- Multi-level parsing with XML aggregation
- Safe escaping helpers (`q.sys_safe`, `q.sys_quote`)

### 5. **Dynamic SQL Variable Engine**
A complete XML‑based system for managing parameters:
- Add variables using type‑specific methods:  
  `q._str`, `q._int`, `q._date`, `q._guid`, `q._xml`, etc.
- Retrieve values back with `_str_`, `_int_`, `_date_`, etc.
- Merge variable sets (`q._merge`)
- Remove or override variables

Variables are stored in XML:

```xml
<vars>
  <item id="1" name="UserID" type="INT" is_null="0">123</item>
  <item id="2" name="Name" type="NVARCHAR" is_null="0">Max</item>
</vars>
```

### 6. **Safe Injection Engine**
The core function `q.sys_inject` supports special syntax inside SQL templates:

| Syntax | Meaning |
|--------|---------|
| `[*Var*]` | Multiline insertion with formatting |
| `[!Var!]` | Insert value raw, no escaping |
| `["Var"]` | Insert NVARCHAR safely |
| `[{Var}]` | QUOTENAME insertion |
| `[#Var#]` | Typed value injection (INT, DATE, XML, GUID, etc.) |

Example:

```sql
SELECT * FROM Users WHERE ID = [#UserID#]
```

### 7. **Query Extraction (`q._get`)**
Allows writing SQL **inside comments of a stored procedure**, then extracting them dynamically:

```sql
/*≡≡≡--
QueryUser
SELECT * FROM Users WHERE ID = [#UserID#]
--≡≡≡*/
```

Call:

```sql
SELECT q._get('QueryUser', 'dbo.SomeProcedure', @vars)
```

This transforms comment‑embedded SQL into an executable query with variable injection applied.

---

## 📁 Example Usage

```sql
DECLARE @vars XML = NULL;

SET @vars = q._int(@vars, 'UserID', 42);
SET @vars = q._str(@vars, 'Status', 'ACTIVE');

DECLARE @sql NVARCHAR(MAX) = q._get('QueryUser', 'dbo.spUserSearch', @vars);

EXEC(@sql);
```

---

## 📦 Installation

Simply run the `QueryBuilder.sql` script in your target database:

```sql
:r QueryBuilder.sql
```

Ensure:
- You have sufficient permission to create schemas, functions, and procedures.
- TRUSTWORTHY mode is enabled (script sets this automatically).

---

## 🧑‍💻 Author

**Hegel Maxim**  
Developer & Architect of the Q‑Framework  
2022–2023

---

## 📄 License

MIT License (recommended — adjust if needed)
