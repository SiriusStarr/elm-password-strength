module Rumkin.Frequency exposing (frequencyList)

{-| This module should not be needed for typical use of `Rumkin`.

`Rumkin.Frequency` is a lookup table used by `Rumkin`. It is adapted from the original source available here at <http://rumkin.com/tools/password/passchk.php>


## Frequency List

@docs frequencyList

-}


{-| A compressed and encoded letter frequency table.

The frequency thing is a bit more interesting, but still not too complex.
Each three letters are base-95 encoded number representing the chance that
this combination comes next. Subtract the value of ' ' from each of the
three, then ((((first\_value \* 95) + second\_value) \* 95) + third\_value) will
give you the odds that this pair is grouped together. The first is " "
(non-alpha chars), then " a", " b", etc. " y", " z", "a ", "aa", "ab", and
so on. If you decrypt the table successfully, you should see a really large
number for "qu".

-}
frequencyList : String
frequencyList =
    "gL6-A$A:#@'$KT#<c\"06\"8*\"Wb\"5-$Kr t%#f+\"S4$7;$ v!R=%!6 g@#s}&'P#rM\"a8\"K'!3= #V }o wx*bZ\"X=\"De#2 #~B!dL!\\\\\"(u qj#Pf L_\"e (1K$9--x6 SE\"x- .$)/s%rK(w\"\"QV\"oQ ;; {_ [| q@(8t-G7#nU MW cS2$9 UJ GH <]*#S !: >!'kQ \\Z @A)O8 5& E (L\"\"YZ r_&g2 66 ~& *L!n* zO,|Y)B8 V.#X: qI'V0 B7 s@4<B)>= I-$wt!;A =h ?G+y: p/ |k\"}} &w\"]?#72 W) j$ @w S !I700V'gf (Q cf!9{9cL Qa .i #Z, F *M NZ!0c RX!+%((N S$ ]o#hU#{o Z2#j_ 6P d% I!!wX _Z.nH!i.!@y!6T#a$\"Yt ?]!kv Jl\"]{ sh!2$&x~\"5F.8X AF!n2 :A1G@('.%5} 24!TZ XY \\P zq G,).J)>} GD _o Xu*di&m0!%Q  L-KA @I ei%4P <S 8m+NI l& Gg&m+\"nS$T{%&? YT e( Mu g2 ~., B(\\P N6 .d .M:^6 gg\"]s\"k2':6 LU /h#k_ 6-#$3%p9 W$ 7x&)(%_-\">[#Q8 VM y( 3q c* vn/va-K> ^N Il LQ/y9 qF *< /k+^O zp K;\"bZ!M\\!>(*NW L' .-#=<\"Uz$^:$o? C] F' /)\"+) FC+8H$M&!l1&2'#n\"&K<!d-#\"_ jw A{ J(#~e$Yi#(v.Ng#NF!V8 zM\"Q4*RH&.X E\"\"n; ^5 41 gM!xj(\"Q1![ bn #!\";?1la yU mR A6)GK @=\"w5!k$!bX\"b[)K-!R) Mq Fl&uz Zx&p\\ <A X' f&!B_ xj)GW.3F <G *5 .e-}6 (6 C-!U>)S< :r#=;#:C 9Q!xJ,7< w\" Tp#*1%|F#Ze&JW Dn /$ ](\"4( H{&>!.@h W[ .F\"G}/#k :_ ,U ;$04E \\,!/P)h9!Hq f4( f S& 18 M^\":'#w|#]o 7= !# Y]\"*4 YZ-<E0$]\"B^ QO bn/$G )> zO e>-|+ \\? SY (d$nj b%)p+%aQ 2P oa!U\\ mp$x: S: *$ &.!9( 3-3KD&WW WG\"jB'`),:t I<'T_ I=($C Pd!;M 5I 3a\"h %_G $, .E (_%wZ(WC!*] w> \"$ s} WT h;*Kd UY!JW\")u\",)\"v6!AU\"Mv RS\"hZ \"(\"Ya&e/%5S-w\\\"/W#b1 lF*fp%a6$3c$aH#uf!e> cH!]v 58*En+r< Ic v< gh,&\\ +A ;a$2!)I. !8 :w$>E 2Q fH+LA$)$ Gl+NL\"y9\"43$30 \"8 d% 8l u% cV$pm 6C 2O uH .S K@ $= DB B(!f6 =9 <R *I uH A@ dG )A `K NX 3L !sopL %: o1 h0 9/ (U(5?-Q8!&=!iX\"f(0\\[ ~V!ii )\\,<? 2W!l*!}#\"\\8#NX)pn S0 Iv!{M$Rs#A2#s\"!$R k& ]J!Yi {#4jR%m` --$\\D ;(*>X ]K lS\"H~'c; ;K#hP!/^!(l !S#z+##d [H F\\%Z3/b-#)f /O c% 66!YO 2w+{3,}? {N I1  O3^6 !? xV\"s5-3I `R wn (E wj Kq';: KA XU&_'#b|$<N$2C qX w# iu!yO Fr($1\"T@\"pg\"eA#+3'g<!w$\":w `7$\\ !Z)#qf%I4%$x*pf!kN\"/* T9(Z])+?&O5\"9\"!nP 2< {9 5q R-'0~5)4 ^= j8 us9#/ @1 }3 Ul0+- U6 *K!&? +K!]p()0 Z$ va!e} Y< fl!rQ *) C( KJ#f4 $;'v.5aO W` w*!A<0IC yG ru!KR-*/ <\\ ,-!&3!Ap&Jc'+` V& s5\"gB$gF!`>!B0 hO g. ;w\"I5 |v7:@&:o %h#zz H\"(wx tL rk!q\\*w9 {D Q-!T_!lF!O$%vF&cB 8 \"Mp!SZ'he!HP rq!>N\"BA\"Y3 wR8^U$.~ ee#K[\";,#IZ ;f!KI!eE!XQ d-#s/$JY%Pz%/F$+Y#2$ XX#Ez'5g$z9\"9$!1y vB 51!La @T*\\f0)j ek L3!^@1t- II Zn m@,08 )0!-@!<-! |\"Fp&Oj A& N_ U[ P>!-h$D` 3a!K' `d#uc%"
