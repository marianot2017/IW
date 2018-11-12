*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005T.PRG -> Convenio Multilateral por coeficiente
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_pref,w_call

private all like w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog =upper(prompt())
w_alta =.f.
w_pfile='05t'
w_title=w_prog
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

use (d0f00) in 0 alias 'f00'							  	&&(Parametros)
use (d0f01) in 0 alias 'f01'  order 'd1tip'		  	&&(Tablas)
use (d0f01) in 0 alias 'f01t' order 'd1tip' again 	&&(Tablas)
use (d0f02) in 0 alias 'f02'  order 'd2clie'		  	&&(Clientes)
use (d0f04) in 0 alias 'f04'  order 'd4peri'		  	&&(Periodos)
use (d0f05) in 0 alias 'f05'  order 'd5peri' exclu	&&(Comprobantes)
use (d0f13) in 0 alias 'f13'  order 'd13peri'	  	&&(Anticipos y Retenciones)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

sele (w_alias)

*) Variables-------------------------------------------------------------------

go bottom in f04
wdperi=f04.d4peri
whperi=f04.d4peri
wdacum=wdperi

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005t.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf
dele file &d0f99.c.cdx
dele file &d0f99.b.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05t
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05t0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05t0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid05t(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

t_leye='Perํodo '+wdperi+' al '+whperi+', Acum.Desde:'+wdacum+', CUIT: '+f00.p0cui+', IB:'+f00.p0nuib

_wdperi=ptoi(wdperi)
_whperi=ptoi(whperi)
_wdacum=ptoi(wdacum)

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
	dele file &d0f99.c.cdx
endif

sele 0
select *;
	from f05;
	where d5tipo='V' and;
	between(ptoi(d5peri),_wdacum,_whperi);
	into dbf &d0f99.c order by d5juri
use &d0f99.c alias f99c exclusive

*set relation to 'JU'+d5juri  into f01
set relation to 'RV'+d5rubr  into f01t additive

sele f99c
index on d5tipo+ptoi(d5peri)+d5juri+d5rubr tag 'd5peri'

w_caju=0	&& cantidad jurisdicciones nuevas
w_junu=0	&& array con jurisdicciones nuevas

do vent05t

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

do repdest &&(Setea variables internas del reporte)

sele f99b
go top

if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr005t'
	=rep_form()
	p_report=''
endif

if w_caju>0	&& si la jurisd.central es nueva debe sacar 1บejerccicio
	for w_i=1 to w_caju
		=seek('JU'+w_junu[w_i],'f01')
		if d0juce=w_junu[w_i]
			if empty(f01.d1coef) && debe sacar 1บejercicio
				w_caju=0	
				exit
			endif
		endif
	endfor
endif

if w_caju>0	&& hay jurisdicciones nuevas
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
		dele file &d0f99.c.cdx
	endif
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.b.dbf
	endif
	if w_caju=1
		=advsuave('Hay jurisdicci๓n nueva. A continuaci๓n saldrแ su liquidaci๓n.')
	else
		=advsuave('Hay jurisdicciones nuevas. A continuaci๓n saldrแn las liquidaciones.')
	endif
	for w_i=1 to w_caju
		do iwp005i with (w_junu[w_i]),.t.	&& calculo para jurisdicciones nuevas
	endfor
	sele(w_alias)
endif

do represet &&(Resetea estado)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc when05t
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc valid05t
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='WDPERI' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	if !seek(ptoi(wdperi),'f04')
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
	if !seek(ptoi(whperi),'f04')
		if !w_show
			wait window 'Perํodo Inexistente'
		endif
		w_retu=.f.
	endif
	if wdperi>whperi
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
	if !seek(ptoi(wdacum),'f04')
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

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc vent05t
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if used('f99b')
	sele f99b
	zap
else
	sele 0
	create dbf &d0f99.b (d5juri c(1),d5titu c(90),d5base n(15,2),d5alic n(5,2),d5deal c(6);
		,d5impu n(15,2),d5line l(1),d5vaba l(1),d5vaal l(1),d5vaim l(1))
	use &d0f99.b alias f99b exclusive
endif

gtotacum=0 &&(Gran total de acumulados)
gtotperi=0 &&(Gran total del periodo pediso)

stor 0 to wttimp1,wttimp2,wttimp3,wttimp4,;
	wtoanp1,wtoanp2,wtoanp3,wtoanp4

w_caju=0	&& cantidad jurisdicciones nuevas
sele f99c
go top
do while ptoi(d5peri)<=_whperi and !eof()

	=seek('JU'+d5juri,'f01')
	if empty(f01.d1coef) &&(Saltea las prov. c/coeficiente=0)
		if w_caju=0 or w_junu[w_caju]<>d5juri
			w_caju=w_caju+1
			dimension w_junu(w_caju)
			w_junu[w_caju]=d5juri	&& array con jurisdicciones nuevas
		endif
		skip
		loop
	endif

	*)Totales para el Periodo rangeado
	*)--------------------------------
	if ptoi(d5peri) >= _wdperi
		do case
		case f01t.d1tasa='1'
			wttimp1=wttimp1+d5imp1+d5imp2 &&(Acumulados desde mes 01)
		case f01t.d1tasa='2'
			wttimp2=wttimp2+d5imp1+d5imp2
		case f01t.d1tasa='3'
			wttimp3=wttimp3+d5imp1+d5imp2
		case f01t.d1tasa='4'
			wttimp4=wttimp4+d5imp1+d5imp2
		endcase
		gtotperi=gtotperi+d5imp1+d5imp2
	endif

	*)Totales desde el Acumudalo
	*)--------------------------
	do case
	case f01t.d1tasa='1'
		wtoanp1=wtoanp1+d5imp1+d5imp2
	case f01t.d1tasa='2'
		wtoanp2=wtoanp2+d5imp1+d5imp2
	case f01t.d1tasa='3'
		wtoanp3=wtoanp3+d5imp1+d5imp2
	case f01t.d1tasa='4'
		wtoanp4=wtoanp4+d5imp1+d5imp2
	endcase
	gtotacum=gtotacum+d5imp1+d5imp2

	skip

enddo

stor 0 to wtaimp1,wtaimp2,wtaimp3,wtaimp4,;
	wtt1mp1,wtt2mp2,wtt3mp3,wtt4mp4,;
	wta1mp1,wta2mp2,wta3mp3,wta4mp4,;
	wtoact1,wtoact2,wtoact3,wtoact4

wexis=start(_wdacum,alias())

wtofl=.f.
sele f01
go top
=start('JU','f01')

j_flag=.t. &&(Este flag se utuliza para forzarlo a realizar una pasada +)
d0poju=iif(d0poce=0 .or. d0poce=100,100,100-d0poce)

do while f01->d1tip='JU' and !eof('f01') and p_cancela

	wjuri=alltrim(f01.d1cla)
	if !p_cancela or (d0juce=alltrim(f01->d1cla) and j_flag)
		j_recno=recno('f01')
		skip in f01
		if f01->d1tip<>'JU' or eof('f01') &&(Fuerzo Pasada para Central)
			j_flag=.f.
			go j_recno in f01
			d0poju=100
		endif
		loop
	endif

	*)Base imponible acumulada
	*)------------------------

	btimp1=round(f01->d1coef*wtoanp1/100,2)
	btimp2=round(f01->d1coef*wtoanp2/100,2)
	btimp3=round(f01->d1coef*wtoanp3/100,2)
	btimp4=round(f01->d1coef*wtoanp4/100,2)

	*)Porcentaje Jurisdiccion
	*)-----------------------
	jtimp1=(d0poju*btimp1/100) + iif(!j_flag,wtoact1,0)
	jtimp2=(d0poju*btimp2/100) + iif(!j_flag,wtoact2,0)
	jtimp3=(d0poju*btimp3/100) + iif(!j_flag,wtoact3,0)
	jtimp4=(d0poju*btimp4/100) + iif(!j_flag,wtoact4,0)

	*)Impuesto acumulado
	*)------------------
	itimp1=f01->d1por1*jtimp1/100
	itimp2=f01->d1por2*jtimp2/100
	itimp3=f01->d1por3*jtimp3/100
	itimp4=f01->d1por4*jtimp4/100

	if j_flag
		*)Acumulo para Casa Emisora
		*)-------------------------
		wtoact1=wtoact1+(btimp1-jtimp1)
		wtoact2=wtoact2+(btimp2-jtimp2)
		wtoact3=wtoact3+(btimp3-jtimp3)
		wtoact4=wtoact4+(btimp4-jtimp4)
	endif

	wtojui=itimp1+itimp2+itimp3+itimp4 &&(Totales de los impuestos)

	if p_cancela and !empty(btimp1+btimp2+btimp3+btimp4)

		insert into f99b values;
			(wjuri,alltrim(f01.d1cla)+' '+f01.d1des,0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
			(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
			(wjuri,'RUBRO 1 ( DORSO )',0,0,'',0,.f.,.f.,.f.,.f.)

		*)Base imponible acumulada   *)Alกcuota   *)Impuesto acumulado
		*)------------------------   *)--------   *)------------------
		if !empty(jtimp1)
			insert into f99b values;
				(wjuri,'Actividad 1 : '+'(31) ',jtimp1,f01.d1por1,'% (51)',itimp1,.f.,.t.,.t.,.t.)
		endif

		if !empty(jtimp2)
			insert into f99b values;
				(wjuri,'Actividad 2 : '+'(32) ',jtimp2,f01.d1por2,'% (52)',itimp2,.f.,.t.,.t.,.t.)
		endif

		if !empty(jtimp3)
			insert into f99b values;
				(wjuri,'Actividad 3 : '+'(33) ',jtimp3,f01.d1por3,'% (53)',itimp3,.f.,.t.,.t.,.t.)
		endif

		if !empty(jtimp4)
			insert into f99b values;
				(wjuri,'Actividad 4 : '+'(34) ',jtimp4,f01.d1por4,'% (54)',itimp4,.f.,.t.,.t.,.t.)
		endif

		*)Sumatoria de Anticipos y Retenciones
		*)------------------------------------

		sele f13
		sum d13anti,d13rete,d13otpa to w5anti,w5rete,w5otpa;
			for between(ptoi(d13peri),_wdacum,str(val(_wdperi)-1,6)) and f13.d13juri$wjuri

		sele f01
		wtores=wtojui- (w5anti+w5rete+w5otpa)

		insert into f99b values;
			(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)

		insert into f99b values;
			(wjuri,space(38)+'Subtotal................: ',0,0,'',wtojui,.f.,.f.,.f.,.t.)

		insert into f99b values;
			(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)


		insert into f99b values;
			(wjuri,'COEFICIENTE    :     '+transform(f01.d1coef,'9,999,999,999.99')+' Anticipo Acumulado......: ',0,0,'',w5anti,.f.,.f.,.f.,.t.)
*		insert into f99b values;
			(wjuri,'COEFICIENTE    : '+space(21)+'Anticipo Acumulado......: ',f01.d1coef,0,'',w5anti,.f.,.t.,.f.,.t.)
*		insert into f99b values;
			(wjuri,space(38)+'Anticipo Acumulado......: ',0,0,'',w5anti,.f.,.f.,.f.,.t.)

		insert into f99b values;
			(wjuri,space(38)+'Retenci๓n Acumulada.....: ',0,0,'',w5rete,.f.,.f.,.f.,.t.)

		insert into f99b values;
			(wjuri,'TOTAL DEL PAIS :     '+transform((wttimp1+wttimp2+wttimp3+wttimp4),'9,999,999,999.99')+' Otros Pagos.............: ',0,0,'',w5otpa,.f.,.f.,.f.,.t.)
*		insert into f99b values;
			(wjuri,'TOTAL DEL PAIS : '+space(21)+'Otros Pagos.............: ',(wttimp1+wttimp2+wttimp3+wttimp4),0,'',w5otpa,.f.,.t.,.f.,.t.)
*		insert into f99b values;
			(wjuri,space(38)+'Otros Pagos.............: ',0,0,'',w5otpa,.f.,.f.,.f.,.t.)

*		insert into f99b values;
			(wjuri,'COEFICIENTE    : ',f01.d1coef,0,'',0,.f.,.t.,.f.,.f.)
*		insert into f99b values;
			(wjuri,'TOTAL DEL PAIS : ',(wttimp1+wttimp2+wttimp3+wttimp4),0,'',0,.f.,.t.,.f.,.f.)

		insert into f99b values;
			(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)
		insert into f99b values;
			(wjuri,space(29)+'Saldo al Frente -RUBRO 2-ITEM 21 : ',0,0,'',wtores,.f.,.f.,.f.,.t.)
		insert into f99b values;
			(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)
		insert into f99b values;
			(wjuri,'RUBRO 2 ( FRENTE )',0,0,'',0,.f.,.f.,.f.,.f.)


		*)Sumatoria de Anticipos y Retenciones para el Perกodo rangeado
		*)-------------------------------------------------------------

		sele f13
		sum d13anti,d13rete,d13otpa to w5anti, w5rete, w5otpa   ;
			for between(ptoi(d13peri),_wdperi,_whperi) and d13juri$wjuri
			
		sele f01
		wtorem=wtores- (w5anti+w5rete+w5otpa)

		insert into f99b values;
			(wjuri,space(38)+'Total RUBRO  1......(21): ',0,0,'',wtores,.f.,.f.,.f.,.t.)
		insert into f99b values;
			(wjuri,space(38)+'Pagos a Cta del Mes.(22): ',0,0,'',w5anti,.f.,.f.,.f.,.t.)
		insert into f99b values;
			(wjuri,space(38)+'Rete/Percep del Mes.(23): ',0,0,'',w5rete,.f.,.f.,.f.,.t.)
		insert into f99b values;
			(wjuri,space(38)+'Otros Pagos del Mes.(24): ',0,0,'',w5otpa,.f.,.f.,.f.,.t.)
		insert into f99b values;
			(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)
		insert into f99b values;
			(wjuri,space(38)+'Total............(25/26): ',0,0,'',wtorem,.f.,.f.,.f.,.t.)

		*)Base imponible del Perกodo   * Alกcuota   * Impuesto del Mes
		*)--------------------------   * --------   * ----------------
		wtimp1= round(f01->d1coef*wttimp1/100,2)
		wtimp2= round(f01->d1coef*wttimp2/100,2)
		wtimp3= round(f01->d1coef*wttimp3/100,2)
		wtimp4= round(f01->d1coef*wttimp4/100,2)

		wtaimp1=wtimp1*f01->d1por1/100
		wtaimp2=wtimp2*f01->d1por2/100
		wtaimp3=wtimp3*f01->d1por3/100
		wtaimp4=wtimp4*f01->d1por4/100

		insert into f99b values;
			(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
			(wjuri,'Base Imponible del Perํodo : '+wdperi+' / '+whperi,0,0,'',0,.f.,.f.,.f.,.f.)

		if !empty(wtimp1)
			insert into f99b values;
				(wjuri,'Actividad 1 : '+'(31) ',wtimp1,f01.d1por1,'% (51)',wtaimp1,.f.,.t.,.t.,.t.)
		endif

		if !empty(wtimp2)
			insert into f99b values;
				(wjuri,'Actividad 2 : '+'(32) ',wtimp2,f01.d1por2,'% (52)',wtaimp2,.f.,.t.,.t.,.t.)
		endif

		if !empty(wtimp3)
			insert into f99b values;
				(wjuri,'Actividad 3 : '+'(33) ',wtimp3,f01.d1por3,'% (53)',wtaimp3,.f.,.t.,.t.,.t.)
		endif

		if !empty(wtimp4)
			insert into f99b values;
				(wjuri,'Actividad 4 : '+'(34) ',wtimp4,f01.d1por4,'% (54)',wtaimp4,.f.,.t.,.t.,.t.)
		endif

		wtomes=wtaimp1+wtaimp2+wtaimp3+wtaimp4

		insert into f99b values;
			(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)

		insert into f99b values;
			(wjuri,space(38)+'Total Perํodo...........: ',0,0,'',wtomes,.f.,.f.,.f.,.t.)

		insert into f99b values;
			(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		wtofl=.t. &&(flag de impresion de tot gen)

	endif

	skip in f01

	if !j_flag &&(Fuerzo salida Termino Listado)
		exit
	endif

	if f01->d1tip<>'JU' or eof('f01') &&(Fuerzo Pasada para Central)
		j_flag=.f.
		go j_recno in f01
		d0poju=100
	endif

enddo

if !empty(gtotacum)
	insert into f99b values;
		(wjuri,replicate('.',90),0,0,'',0,.f.,.f.,.f.,.f.)
	insert into f99b values;
		(wjuri,'Sumatoria de Base Imponible Acumulada   : ',0,0,'',gtotacum,.f.,.f.,.f.,.t.)
endif
if !empty(gtotperi)
	insert into f99b values;
		(wjuri,replicate('.',90),0,0,'',0,.f.,.f.,.f.,.f.)
	insert into f99b values;
		(wjuri,'Sumatoria de Base Imponible del Perํodo : ',0,0,'',gtotperi,.f.,.f.,.f.,.t.)
endif
if !empty(gtotacum) or !empty(gtotperi)
	insert into f99b values;
		(wjuri,replicate('.',90),0,0,'',0,.f.,.f.,.f.,.f.)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Puesto adrede para que incluya el report al proyecto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
report form iwr005t
