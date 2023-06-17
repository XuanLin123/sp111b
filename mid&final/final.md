# C4-compiler-code

## Tokens
```
30 // tokens and classes (operators last and in precedence  order)
31 enum {
32   Num = 128, Fun, Sys, Glo, Loc, Id,
33   Char, Else, Enum, If, Int, Return, Sizeof, While,
34   Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt,  Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
35 };
```
* 在不同階段用於語法、語意分析
* Token 依照優先順序排序
* 每個Token都對應一個ASCII Code (0~127是ASCII Code的定義範圍，所以從128開始)

---

## Opcodes
```
37 // opcodes
38 enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
39        OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
40        OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };
```
* 控制虛擬機的控制碼
* 每個Token功能參考 [C4的虛擬機](https://github.com/ccc111b/cpu2os/blob/master/A4-%E5%8F%83%E8%80%83%E5%B0%88%E6%A1%88/compiler/c4/C4%E7%B7%A8%E8%AD%AF%E5%99%A8%E4%B8%AD%E7%9A%84%E8%99%9B%E6%93%AC%E6%A9%9F.md)

---

## Types
```
42 // types
43 enum { CHAR, INT, PTR };
```
* C4使用的三種型態

---

## Identifier offsets
```
45 // identifier offsets (since we can't create an ident struct)
46 enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };
```
* C4無法使用Struct
* id[Tk] 代表 id.Tk , id[Hash] 代表 id.Hash , id[Name] 代表 id.Name

---

## Analyzer

C4的重點部分

`void next() void expr() void stmt()`
* void next() : 用於詞法分析
* void expr() : 用於語法分析
* void stmt() : 用於語意分析

---

## next() : 詞法分析

```
48  void next() 
    {
    .
    .
    .
132 }
```

### while

* 這個迴圈會讀入程式碼的所有字符，之後依照讀入的內容執行相對應的動作
* 迴圈內透過多種if-else分類 (數字、字母、註解、換行...)
* 識別不同的Token
```
void next()
{
  char *pp;

  while (tk = *p) {
      .
      .
      .
  }
```

### 換行

* 在62行判斷指令在ADJ的前還是後，依結果加入換行
```
54   if (tk == '\n') {
55       if (src) {
56         printf("%d: %.*s", line, p - lp, lp);
57         lp = p;
58         while (le < e) {
59           printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
60                            "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
61                            "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
62           if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
63         }
64       }
65       ++line;
66    }
```

### 跳過 # 開頭

* 跳過 # 開頭，遇到"0"或"換行"就結束
```
67  else if (tk == '#') {
68     while (*p != 0 && *p != '\n') ++p;
```

### 大小寫A-Z 和 _

* 識別和處理identifier
* 檢查字符是否符合條件，並且轉換成數值
* 確定id位置
```
70    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
71       pp = p - 1;
72       while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
73         tk = tk * 147 + *p++;
74       tk = (tk << 6) + (p - pp);
75       id = sym;
76       while (id[Tk]) {
77         if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
78         id = id + Idsz;
79       }
80       id[Name] = (int)pp;
81       id[Hash] = tk;
82       tk = id[Tk] = Id;
83       return;
84    }
```

### 0-9

* 識別和處理數字
* 轉換進位制
```
85     else if (tk >= '0' && tk <= '9') {
86       if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
87       else if (*p == 'x' || *p == 'X') {
88         while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
89           ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
90       }
91       else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
92       tk = Num;
93       return;
94     }
```

### /

* 如果 / 後面還是 / 表示該行為註解，跳過
```
95     else if (tk == '/') {
96       if (*p == '/') {
97         ++p;
98         while (*p != 0 && *p != '\n') ++p;
99       }
100      else {
101        tk = Div;
102        return;
103      }
104    }
```

### \ 和 "

* 字元和字串處理
```
105    else if (tk == '\'' || tk == '"') {
106      pp = data;
107      while (*p != 0 && *p != tk) {
108        if ((ival = *p++) == '\\') {
109          if ((ival = *p++) == 'n') ival = '\n';
110        }
111        if (tk == '"') *data++ = ival;
112      }
113      ++p;
114      if (tk == '"') ival = (int)pp; else tk = Num;
115      return;
116    }
```

### 運算符號

* 判斷是否為運算子，是的話設定`tk`
```
117    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
118    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
119    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
120    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
121    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
122    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
123    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
124    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
125    else if (tk == '^') { tk = Xor; return; }
126    else if (tk == '%') { tk = Mod; return; }
127    else if (tk == '*') { tk = Mul; return; }
128    else if (tk == '[') { tk = Brak; return; }
129    else if (tk == '?') { tk = Cond; return; }
130    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
```

---

## expr() : 語法分析

```
134 void expr(int lev)
    {
    .
    .
    .
282 }
```

### 初始檢查

* 檢查`tk`是否為零
```
138    if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
```

### 數字檢查

* 如果 tk = Num 代表遇到數字
```
139    else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
```

### 字串檢查

* 如果 tk = " (單引號) 代表遇到字串
```
140     else if (tk == '"') {
141       *++e = IMM; *++e = ival; next();
142       while (tk == '"') next();
143       data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
144     }
```

### Sizeof

```
145     else if (tk == Sizeof) {
146       next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
147       ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
148       while (tk == Mul) { next(); ty = ty + PTR; }
149       if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
150       *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
151       ty = INT;
152     }
```

### identifier

* 處理函式或變數等情況
```
153   else if (tk == Id) {
154     d = id; next();
155     if (tk == '(') {
156       next();
157       t = 0;
158       while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
159       next();
160       if (d[Class] == Sys) *++e = d[Val];
161       else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
162       else { printf("%d: bad function call\n", line); exit(-1); }
163       if (t) { *++e = ADJ; *++e = t; }
164       ty = d[Type];
165     }
166     else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
167     else {
168       if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
169       else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
170       else { printf("%d: undefined variable\n", line); exit(-1); }
171       *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
172     }
173   }
```

### ( 括號開頭

```
174     else if (tk == '(') {
175       next();
176       if (tk == Int || tk == Char) {
177         t = (tk == Int) ? INT : CHAR; next();
178         while (tk == Mul) { next(); t = t + PTR; }
179         if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
180         expr(Inc);
181         ty = t;
182       }
183       else {
184         expr(Assign);
185         if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
186       }
187     }
```

### * 乘號

* 解析是否為指標
```
188     else if (tk == Mul) {
189       next(); expr(Inc);
190       if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
191       *++e = (ty == CHAR) ? LC : LI;
192     }
```

### & 運算子

* 判斷其是否為取址符號
```
193     else if (tk == And) {
194       next(); expr(Inc);
195       if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
196       ty = ty + PTR;
197     }
```

### ! ~ +

```
198     else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
199     else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
200     else if (tk == Add) { next(); expr(Inc); ty = INT; }
```

### -

```
201     else if (tk == Sub) {
202       next(); *++e = IMM;
203       if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
204       ty = INT;
205     }
```

### 遞增/減 "++" "- -"

```
206     else if (tk == Inc || tk == Dec) {
207       t = tk; next(); expr(Inc);
208       if (*e == LC) { *e = PSH; *++e = LC; }
209       else if (*e == LI) { *e = PSH; *++e = LI; }
210       else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
211       *++e = PSH;
212       *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
213       *++e = (t == Inc) ? ADD : SUB;
214       *++e = (ty == CHAR) ? SC : SI;
215     }
```

### 賦值

```
220   if (tk == Assign) {
221      next();
222      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
223      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
224    }
```

### 三元條件

* 判斷、跳轉、執行
```
225      else if (tk == Cond) {
226         next();
227         *++e = BZ; d = ++e;
228         expr(Assign);
229         if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
230         *d = (int)(e + 3); *++e = JMP; d = ++e;
231         expr(Cond);
232         *d = (int)(e + 1);
233       }
```
### 邏輯

* 邏輯 或/與
```
234       else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
235       else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
```

### 或/互斥或/&

```
236       else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
237       else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
238       else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
```

### 和等於相關的

* "=" "!=" "<" ">" "<=" ">="
```
239       else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
240       else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
241       else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
242       else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
243       else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
244       else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
```

### 左右移

```
245       else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
246       else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
```

### 加法

```
247       else if (tk == Add) {
248         next(); *++e = PSH; expr(Mul);
249         if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
250         *++e = ADD;
251       }
```

### 減法

```
252       else if (tk == Sub) {
253         next(); *++e = PSH; expr(Mul);
254         if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
255         else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
256         else *++e = SUB;
257       }
```

### 乘、除、求餘

```
258       else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
259       else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
260       else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
```

### 後至遞增/減

```
261       else if (tk == Inc || tk == Dec) {
262         if (*e == LC) { *e = PSH; *++e = LC; }
263         else if (*e == LI) { *e = PSH; *++e = LI; }
264         else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
265         *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
266         *++e = (tk == Inc) ? ADD : SUB;
267         *++e = (ty == CHAR) ? SC : SI;
268         *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
269         *++e = (tk == Inc) ? SUB : ADD;
270         next();
271       }
```

### 中括號

```
272       else if (tk == Brak) {
273         next(); *++e = PSH; expr(Assign);
274         if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
275         if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
276         else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
277         *++e = ADD;
278         *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
279       }
```

---

## stmt() : 語意分析

```
284 void stmt()
    {
    .
    .
    .
331 }
```

### if-else

* 檢查是否符合if-else語句
```
288     if (tk == If) {
289       next();
290       if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
291       expr(Assign);
292       if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
293       *++e = BZ; b = ++e;
294       stmt();
295       if (tk == Else) {
296         *b = (int)(e + 3); *++e = JMP; b = ++e;
297         next();
298         stmt();
299       }
300       *b = (int)(e + 1);
301     }
```

### while

* 檢查是否符合while語句
```
302     else if (tk == While) {
303       next();
304       a = e + 1;
305       if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
306       expr(Assign);
307       if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
308       *++e = BZ; b = ++e;
309       stmt();
310       *++e = JMP; *++e = (int)a;
311       *b = (int)(e + 1);
312     }
```

### return

* 沒有回傳值(後接著 ; )，生成"LEV"結束
* 有回傳值，expr(Assign)
```
313     else if (tk == Return) {
314       next();
315       if (tk != ';') expr(Assign);
316       *++e = LEV;
317       if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
318     }
```

### 大括號 { }

* 遇 "{" 進入迴圈， "}" 結束
```
319     else if (tk == '{') {
320       next();
321       while (tk != '}') stmt();
322       next();
323     }
```

### 分號 ;

```
324     else if (tk == ';') {
325       next();
326     }
```

---

## 主程式

### 檢查格式及必要參數

* 格式錯誤或缺少必要參數回傳 -1
```
340     if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
341     if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
342     if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }
```

### 檔案開啟

```
344     if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

```

### malloc

* 分配空間大小(符號、文本、堆疊...)
```
346     poolsz = 256*1024; // arbitrary size
347     if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
348     if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
349     if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
350     if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }
```

### 格式化

```
352     memset(sym,  0, poolsz);
353     memset(e,    0, poolsz);
354     memset(data, 0, poolsz);
```

### 關鍵字和函數字串符

```
356     p = "char else enum if int return sizeof while "
357         "open read close printf malloc free memset memcmp exit void main";
358     i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
359     i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
360     next(); id[Tk] = Char; // handle void type
361     next(); idmain = id; // keep track of main
```

### 數據讀取

```
363     if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
364     if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
365     p[i] = 0;
366     close(fd);
```

### 標記

* 確保與關鍵字是否相符
```
371     while (tk) {
372       bt = INT; // basetype
373       if (tk == Int) next();
374       else if (tk == Char) { next(); bt = CHAR; }
375       else if (tk == Enum) {
 .        .
 .        .
 .        .
459        }
460        next();
461      }

```

### main()

* 檢查main()是否有定義
```
463     if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
464     if (src) return 0;
```

### 主循環

* 虛擬機循環
```
474     // run...
475     cycle = 0;
476     while (1) {
477       i = *pc++; ++cycle;
478       if (debug) {
479         printf("%d> %.4s", cycle,
480           &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
481            "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
482            "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
483         if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
484       }
```

### 指令

```
485       if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
486       else if (i == IMM) a = *pc++;                                         // load global address or immediate
487       else if (i == JMP) pc = (int *)*pc;                                   // jump
488       else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
489       else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
490       else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
491       else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
492       else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
493       else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
494       else if (i == LI)  a = *(int *)a;                                     // load int
495       else if (i == LC)  a = *(char *)a;                                    // load char
496       else if (i == SI)  *(int *)*sp++ = a;                                 // store int
497       else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
498       else if (i == PSH) *--sp = a;                                         // push

500       else if (i == OR)  a = *sp++ |  a;
501       else if (i == XOR) a = *sp++ ^  a;
502       else if (i == AND) a = *sp++ &  a;
503       else if (i == EQ)  a = *sp++ == a;
504       else if (i == NE)  a = *sp++ != a;
505       else if (i == LT)  a = *sp++ <  a;
506       else if (i == GT)  a = *sp++ >  a;
507       else if (i == LE)  a = *sp++ <= a;
508       else if (i == GE)  a = *sp++ >= a;
509       else if (i == SHL) a = *sp++ << a;
510       else if (i == SHR) a = *sp++ >> a;
511       else if (i == ADD) a = *sp++ +  a;
512       else if (i == SUB) a = *sp++ -  a;
513       else if (i == MUL) a = *sp++ *  a;
514       else if (i == DIV) a = *sp++ /  a;
515       else if (i == MOD) a = *sp++ %  a;
```

### 系統操作指令

```
517       else if (i == OPEN) a = open((char *)sp[1], *sp);
518       else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
519       else if (i == CLOS) a = close(*sp);
520       else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
521       else if (i == MALC) a = (int)malloc(*sp);
522       else if (i == FREE) free((void *)*sp);
523       else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
524       else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
525       else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
526       else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
```


## 參考資料
[c4 - C in four functions](https://github.com/rswier/c4)
[C in four function (c4) Compiler](https://hackmd.io/@srhuang/Bkk2eY5ES)    
[c4 -- 編譯器 ccc 加註解後的原始碼版本](https://github.com/ccc111b/cpu2os/blob/master/A4-%E5%8F%83%E8%80%83%E5%B0%88%E6%A1%88/README.md)