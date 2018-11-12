*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005I.PRG -> Convenio Multilateral 1บEjercicio
*)-H
*)-H Release		 : 01
*)-H
*)-H Fecha de inicio : 
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='05i'
w_prog =iif(w_call,'CONVENIO MULTILATERAL 1บ EJERCICIO',upper(prompt()))
w_title=w_prog
w_alta =.f.
w_alias='f05'

w5safa=0
wtojui=0

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F01t'
fieldvec[4]='F02'
fieldvec[5]='F04'
fieldvec[6]='F05'
fieldvec[7]='F13'

if !w_call

	use (d0f00) in 0 alias 'f00'								&&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'			&&(Tablas)
	use (d0f01) in 0 alias 'f01t' order 'd1tip' again	&&(Tablas)
	use (d0f02) in 0 alias 'f02' order 'd2clie'			&&(Clientes)
	use (d0f04) in 0 alias 'f04' order 'd4peri'			&&(Periodos)
	use (d0f05) in 0 alias 'f05' order 'd5peri' exclu	&&(Comprobantes)
	use (d0f13) in 0 alias 'f13' order 'd13peri'			&&(Anticipos y Retenciones)
	
	if neterr()
		pop key
		pop menu _msysmenu
		fieldvec=''
		=closall()
		return
	endif

endif

fieldvec=''

sele (w_alias)

if w_call	&& es invocado del iwp005t (ing.brutos por coeficiente)

	wdjuri=w_pref
	whjuri=w_pref
	do cons05i0		&& R.01 Mariano (antes do cons05i)
		
else
	*) Variables-------------------------------------------------------------------

	wdjuri=' '
	whjuri='Z'
	go bottom in f04
	wdperi=f04.d4peri
	whperi=f04.d4peri
	wdacum=wdperi

	*)-----------------------------------------------------------------------------

	do ssm998.mpr
	do iws005i.spr

endif

pop key
pop menu _msysmenu

if !w_call
	=closall()
	dele file &d0f99.c.dbf
	dele file &d0f99.c.cdx
	dele file &d0f99.b.dbf
endif

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05i0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05i0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !w_call
	if .not. valid05i(.t.)
		wait wind 'Verifique los datos ingresados...'
		return
	endif
	_wdperi=ptoi(wdperi)
	_whperi=ptoi(whperi)
	_wdacum=ptoi(wdacum)
endif

=gentmp()
	
if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
	dele file &d0f99.c.cdx
endif

sele 0
select *;
	from f05;
	where d5tipo='V' and between(ptoi(d5peri),_wdacum,_whperi) ;
			and between(d5juri,wdjuri,whjuri) ;
	into dbf &d0f99.c order by d5juri
use &d0f99.c alias f99c exclusive

*set relation to 'JU'+d5juri  into f01
set relation to 'RV'+d5rubr  into f01t additive

index on d5tipo+d5juri+ptoi(d5peri)+d5rubr tag 'd5juri'

do vent05i

sele f99b
go top
do while !eof()
	scatter memvar
	if m.d5vaba
		m.d5titu=left(m.d5titu,25)+transform(m.d5base,'9,999,999,999.99')+space(50)
	endif
	if m.d5vaal
		m.d5titu=left(m.d5titu,51)+str(m.d5alic,5,2)+' '+m.d5deal+space(40)
	endif
	if m.d5vaim
		m.d5titu=left(m.d5titu,71)+t_rans(m.d5impu)+space(20)
	endif
	if m.d5line
		m.d5titu=left(m.d5titu,70)+'--------------------'+space(20)
	endif
	gather memvar
	skip
enddo
p_repdos=45

sele f99c
set relation to

if !p_cancela
	return
endif

t_leye = 'Perํodo '+wdperi+' al '+whperi+'  Acum.Desde:'+wdacum+'  CUIT:'+f00.p0cui+'  IB:'+f00.p0nuib

do repdest &&(Setea variables internas del reporte)

sele f99b
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr005i'
	=rep_form()
	p_report=''
endif
	

if w_call
	* se guarda el estado para que se conserve despues del represet
	w_p_repout=p_repout

	do represet &&(Resetea estado)

	p_repout=w_p_repout
	show get p_repout
else
	do represet &&(Resetea estado)
endif

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when05i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj



return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid05i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='WDJURI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDJURI','JU','iwp001','f01','d1cla','d1cla','Jurisdicci๓n inexistente',(p_char),'w_retu'
	endif
endif

if w_vari='WHJURI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WHJURI','JU','iwp001','f01','d1cla','d1cla','Jurisdicci๓n inexistente',(p_char),'w_retu'
	endif
	if whjuri<wdjuri
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDPERI' or w_show
   if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif

	wkperi = PtoI(wdperi)

   if ! seek(wkperi,'f04')
		if !w_show	   	
   	   wait window 'Perํodo Inexistente'
   	endif
     	w_retu=.f.
	endif
	
endif

if w_vari='WHPERI' or w_show
   if lastkey()=p_f4
		do fhelp with 'WHPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif

	wkperi = PtoI(whperi)

   if ! seek(wkperi,'f04')
		if !w_show	   	
   	   wait window 'Perํodo Inexistente'
   	endif
     	w_retu=.f.
	endif
	if ptoi(whperi)<ptoi(wdperi)
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDACUM' or w_show
   if lastkey()=p_f4
		do fhelp with 'WDACUM','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif

	wkperi = PtoI(wdacum)

   if ! seek(wkperi,'f04')
		if !w_show	   	
   	   wait window 'Perํodo Inexistente'
   	endif
     	w_retu=.f.
	endif
	
	if wdperi<wdacum and w_retu
		if !w_show
			wait wind 'Acumula debe ser menor o igual que Desde...'
		endif
		w_retu=.f.
	endif
endif

return (w_retu)

*-----------------------------------------------------------------------------
Proc Vent05i
*-----------------------------------------------------------------------------

if used('f99b')
	sele f99b
	zap
else
	sele 0
	create dbf &d0f99.b (d5juri c(1),d5titu c(90),d5base n(15,2),d5alic n(5,2),d5deal c(6);
						,d5impu n(15,2),d5line l(1),d5vaba l(1),d5vaal l(1),d5vaim l(1))
	use &d0f99.b alias f99b exclusive
endif
	
wtofl = .f.    &&

j_flag=.t.     && Este flag se utiliza para forzarlo a realizar una pasada +
j_fla2=.t.
j_recno=0
d0poju=iif(d0poce=0 .or. d0poce = 100,100,100-d0poce)

stor 0 to wtoact1,wtoact2,wtoact3,wtoact4,;
          wtoame1,wtoame2,wtoame3,wtoame4

gtotacum=0   && Gran total de acumulados
gtotperi=0   && Gran total del periodo pediso

sele f99c
go top

do while !eof('f99c')     ;
   .and. p_cancela

   =seek('JU'+d5juri,'f01')

   if !p_cancela .or. (d0juce=alltrim(f01.d1cla) .and. j_flag)
      * Loop para saltear los registros que son de la jurisdiccion central
      if j_fla2
         j_recno=recno('f99c')
         j_fla2=.f.
      endif

      skip in f99c

      if eof('f99c')
         * Si es una sola Juridiccion
         * --------------------------
         j_flag=.f.
         go j_recno in f99c
         d0poju=100
      else
         loop
      endif
   endif

   wjuri = d5juri

   stor 0 to wtimp1,wtimp2,wtimp3,wtimp4,;
             wtmal1,wtmal2,wtmal3,wtmal4
    
   wdefl = .f.
   do while d5juri=wjuri and ;
            PtoI(d5peri) <= _whperi and;
            !eof() and p_cancela

      * Acumulacion segun la jurisdiccion que esta procesando

      wdefl = .t.    && flag de impresion

      if between(PtoI(d5peri),_wdperi,_whperi)

        * Sumatoria del Perกodo pedido
        * ----------------------------
         do case
            case f01t.d1tasa='1'
                wtmal1= wtmal1 + d5imp1 + d5imp2  && totales del Perกodo solamente
            case f01t.d1tasa='2'
                wtmal2= wtmal2 + d5imp1 + d5imp2
            case f01t.d1tasa='3'
                wtmal3= wtmal3 + d5imp1 + d5imp2
            case f01t.d1tasa='4'
                wtmal4= wtmal4 + d5imp1 + d5imp2
         endcase

         gtotperi = gtotperi + d5imp1 + d5imp2

      endif

      do case
         case f01t.d1tasa='1'
             wtimp1= wtimp1 + d5imp1 + d5imp2    && Acumulados desde mes 01
         case f01t.d1tasa='2'
            wtimp2= wtimp2 + d5imp1 + d5imp2
         case f01t.d1tasa='3'
             wtimp3= wtimp3 + d5imp1 + d5imp2
         case f01t.d1tasa='4'
             wtimp4= wtimp4 + d5imp1 + d5imp2
      endcase

      gtotacum = gtotacum + d5imp1 + d5imp2

	   skip

   enddo   && fin de acumulacion segun jurisdiccion
   
*   =seek('JU'+wjuri,'f01')
   
   * Porcentaje Jurisdiccion
   * -----------------------
   jtimp1= (d0poju*wtimp1/100) + iif(!j_flag,wtoact1,0)
   jtimp2= (d0poju*wtimp2/100) + iif(!j_flag,wtoact2,0)
   jtimp3= (d0poju*wtimp3/100) + iif(!j_flag,wtoact3,0)
   jtimp4= (d0poju*wtimp4/100) + iif(!j_flag,wtoact4,0)

   * Impuesto acumulado
   * ------------------
   itimp1= f01.d1por1*jtimp1/100
   itimp2= f01.d1por2*jtimp2/100
   itimp3= f01.d1por3*jtimp3/100
   itimp4= f01.d1por4*jtimp4/100

   if j_flag
      * Acumulo para Casa Emisora
      * -------------------------
      wtoact1= wtoact1 + (wtimp1-jtimp1)
      wtoact2= wtoact2 + (wtimp2-jtimp2)
      wtoact3= wtoact3 + (wtimp3-jtimp3)
      wtoact4= wtoact4 + (wtimp4-jtimp4)
   endif

*----------------------------------
   * Trabajo para el Perกodo pedido
   * ------------------------------

   * Porcentaje Jurisdiccion
   * -----------------------
   ktimp1= (d0poju*wtmal1/100) + iif(!j_flag,wtoame1,0)
   ktimp2= (d0poju*wtmal2/100) + iif(!j_flag,wtoame2,0)
   ktimp3= (d0poju*wtmal3/100) + iif(!j_flag,wtoame3,0)
   ktimp4= (d0poju*wtmal4/100) + iif(!j_flag,wtoame4,0)

   * Impuesto acumulado
   * ------------------
   ltimp1= f01->d1por1*ktimp1/100
   ltimp2= f01->d1por2*ktimp2/100
   ltimp3= f01->d1por3*ktimp3/100
   ltimp4= f01->d1por4*ktimp4/100

   if j_flag
      * Acumulo para Casa Emisora
      * -------------------------
      wtoame1= wtoame1 + (wtmal1-ktimp1)
      wtoame2= wtoame2 + (wtmal2-ktimp2)
      wtoame3= wtoame3 + (wtmal3-ktimp3)
      wtoame4= wtoame4 + (wtmal4-ktimp4)
   endif
*-------

   wtojui= itimp1+itimp2+itimp3+itimp4  && Totales de los impuestos

   if wdefl and p_cancela and !empty(wtimp1+wtimp2+wtimp3+wtimp4)

		insert into f99b values;
		(wjuri,alltrim(f01.d1cla)+' '+f01.d1des,0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)
 
		insert into f99b values;
		(wjuri,'RUBRO 1 ( DORSO )',0,0,'',0,.f.,.f.,.f.,.f.)

      * Base imponible acumulada   * Alicuota   * Impuesto acumulado
      * ------------------------   * --------   * ------------------

      if !empty(wtimp1) or !empty(jtimp1)
			insert into f99b values;
			(wjuri,'Actividad 1 : '+'(31) ',jtimp1,f01.d1por1,'% (51)',itimp1,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtimp2) or !empty(jtimp2)
			insert into f99b values;
			(wjuri,'Actividad 2 : '+'(32) ',jtimp2,f01.d1por2,'% (52)',itimp2,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtimp3) or !empty(jtimp3)
			insert into f99b values;
			(wjuri,'Actividad 3 : '+'(33) ',jtimp3,f01.d1por3,'% (53)',itimp3,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtimp4) or !empty(jtimp4)
			insert into f99b values;
			(wjuri,'Actividad 4 : '+'(34) ',jtimp4,f01.d1por4,'% (54)',itimp4,.f.,.t.,.t.,.t.)
      endif

      * Sumatoria de Anticipos y Retenciones
      * ------------------------------------

      sele f13

      sum d13anti,d13rete,d13otpa to w5anti, w5rete, w5otpa   ;
      for between(PtoI(d13peri),_wdacum,str(val(_wdperi)-1,6)) and;
          f13.d13juri=wjuri

		sele f99c

      wtores=wtojui- (w5anti+w5rete+w5otpa)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,space(38)+'Subtotal................: ',0,0,'',wtojui,.f.,.f.,.f.,.t.)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,space(38)+'Anticipo Acumulado......: ',0,0,'',w5anti,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Retenci๓n Acumulada.....: ',0,0,'',w5rete,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Otros Pagos.............: ',0,0,'',w5otpa,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)
		insert into f99b values;
		(wjuri,space(29)+'Saldo al Frente -RUBRO 2-ITEM 21 : ',0,0,'',wtores,.f.,.f.,.f.,.t.)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,'RUBRO 2 ( FRENTE )',0,0,'',0,.f.,.f.,.f.,.f.)

      * Sumatoria de Anticipos y Retenciones para el Perกodo rangeado
      * -------------------------------------------------------------

      sele f13

      sum d13anti,d13rete,d13otpa to w5anti, w5rete, w5otpa   ;
      for between(PtoI(d13peri),_wdperi,_whperi) and d13juri=wjuri

      sele f99c

      wtorem=wtores- (w5anti+w5rete+w5otpa)

		insert into f99b values;
		(wjuri,space(38)+'Total RUBRO  1......(21): ',0,0,'',wtores,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Anticipo del Mes....(22): ',0,0,'',w5anti,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Pagos a Cta del Mes.(23): ',0,0,'',w5otpa,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Perc/Retenc del Mes.(24): ',0,0,'',w5rete,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)
		insert into f99b values;
		(wjuri,space(38)+'Total............(25/26): ',0,0,'',wtorem,.f.,.f.,.f.,.t.)

      * Base imponible acumulada   * Alicuota   * Impuesto acumulado Mes Pedido
      * ------------------------   * --------   * ------------------ --- ------

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,'Base Imponible del Perํodo : '+wdperi+' / '+whperi,0,0,'',0,.f.,.f.,.f.,.f.)

      if !empty(wtmal1) .or. !empty(ktimp1)
			insert into f99b values;
			(wjuri,'Actividad 1 : '+'(31) ',ktimp1,f01.d1por1,'% (51)',ltimp1,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtmal2) .or. !empty(ktimp2)
			insert into f99b values;
			(wjuri,'Actividad 2 : '+'(32) ',ktimp2,f01.d1por2,'% (52)',ltimp2,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtmal3) .or. !empty(ktimp3)
			insert into f99b values;
			(wjuri,'Actividad 3 : '+'(33) ',ktimp3,f01.d1por3,'% (53)',ltimp3,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtmal4) .or. !empty(ktimp4)
			insert into f99b values;
			(wjuri,'Actividad 4 : '+'(34) ',ktimp4,f01.d1por4,'% (54)',ltimp4,.f.,.t.,.t.,.t.)
      endif

      wtomes=ltimp1+ltimp2+ltimp3+ltimp4

		insert into f99b values;
		(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,space(38)+'Total Perํodo...........: ',0,0,'',wtomes,.f.,.f.,.f.,.t.)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

   endif

   if !j_flag    && Fuerzo salida Termino Listado
      exit
   endif

   if eof()
      j_flag=.f.
      if !empty(j_recno)
         go j_recno
      endif
      d0poju=100
   endif

enddo

if !eof('f99b')
	insert into f99b values;
	(wjuri,replicate('.',90),0,0,'',0,.f.,.f.,.f.,.f.)
	if !empty(gtotacum)
		insert into f99b values;
		(wjuri,'Sumatoria de Base Imponible Acumulada   : ',0,0,'',gtotacum,.f.,.f.,.f.,.t.)
	endif
	insert into f99b values;
	(wjuri,replicate('.',90),0,0,'',0,.f.,.f.,.f.,.f.)
	if !empty(gtotperi)
		insert into f99b values;
		(wjuri,'Sumatoria de Base Imponible del Perํodo : ',0,0,'',gtotperi,.f.,.f.,.f.,.t.)
	endif
	insert into f99b values;
	(wjuri,replicate('.',90),0,0,'',0,.f.,.f.,.f.,.f.)
endif

return

***puesto adrede para que incluya el report al proyecto**
report form iwr005i

