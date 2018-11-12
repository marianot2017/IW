*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005M.PRG -> Liquidacion I.B. Alกcuota Fija.
*)-H
*)-H Fecha de inicio : 
*)-H Fecha de fin    :
*)-H
*)-H Release		 : 01
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

w_pfile='05m'
w_prog =upper(prompt())
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

fieldvec=''

sele (w_alias)

*) Variables-------------------------------------------------------------------

go bottom in f04
w5peri=f04.d4peri
wstatus=15

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005m.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf
dele file &d0f99.c.cdx
dele file &d0f99.b.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05m
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05m0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05m0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid05m(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
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
	where d5peri=w5peri and d5tipo='V' ;
	into dbf &d0f99.c order by d5juri
use &d0f99.c alias f99c exclusive

*set relation to 'JU'+d5juri  into f01
set relation to 'RV'+d5rubr  into f01t additive
set relation to ptoi(d5peri) into f13  additive

*index on d5juri+f01t.d1tasa tag 'd5juri'
index on d5tipo+d5juri+f01t.d1tasa+ptoi(d5peri)+d5rubr tag 'd5juri'

do vent05m

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

t_leye = 'Perํodo : '+w5peri+'   Nบ de Cuit : '+f00.p0cui+'  Nบ de Ing.Brutos : '+f00.p0nuib

do repdest &&(Setea variables internas del reporte)

sele f99b
go top

if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr005m'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)


return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when05m
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid05m
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='W5PERI' or w_show
	if lastkey()=p_f4
			do fhelp with 'W5PERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

	w_fech	= CTOD('01/' + w5peri)
	IF EMPTY(w_fech)
			RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
			w5peri	= RIGHT(DTOC(w_fech),7)
			SHOW GET w5peri
	ELSE
	    IF !w_show
			WAIT WIND "El perํodo no es correcto"
	    ENDIF
	    w_retu	= .F.
	    _curobj	= objnum(w5peri)
	    RETURN .F.
	ENDIF
	wkperi = ptoi(ALLT(w5peri))

	if ! seek(wkperi,'f04')
			if !w_show
				wait window 'Perํodo Inexistente'
			endif
			w_retu=.f.
	endif
endif


return (w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Proc Vent05m
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
	
wtofl = .f.    &&

sele f99c
go top

do while !eof()     ;
   .and. p_cancela

   wjuri = d5juri
   
   =seek('JU'+d5juri,'f01')
   
   stor 0 to wtimp1,wtimp2,wtimp3,wtimp4
    
   stor 0 to w5mini,w5rete,w5otpa,w5safa

   wdefl = .f.
   do while d5juri=wjuri and ;
            PtoI(d5peri) = ptoi(w5peri) and;
            !eof() and p_cancela

      * Acumulacion segun la jurisdiccion que esta procesando

      wdefl = .t.    && flag de impresion

      do case
         case f01t.d1tasa='1'
             wtimp1= wtimp1 + d5imp1 + d5imp2
         case f01t.d1tasa='2'
             wtimp2= wtimp2 + d5imp1 + d5imp2
         case f01t.d1tasa='3'
             wtimp3= wtimp3 + d5imp1 + d5imp2
         case f01t.d1tasa='4'
             wtimp4= wtimp4 + d5imp1 + d5imp2
      endcase

	   skip

   enddo   && fin de acumulacion segun jurisdiccion

&& R.01b Ra๚l
*   if !bof('f99c')
*		  	skip -1 in f99c
*   endif
&& R.01e Ra๚l
      
   itimp1= f01.d1por1*wtimp1/100
   itimp2= f01.d1por2*wtimp2/100
   itimp3= f01.d1por3*wtimp3/100
   itimp4= f01.d1por4*wtimp4/100
   
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

      if !empty(wtimp1)
			insert into f99b values;
			(wjuri,'Actividad 1 : '+'(31) ',wtimp1,f01.d1por1,'% (51)',itimp1,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtimp2)
			insert into f99b values;
			(wjuri,'Actividad 2 : '+'(32) ',wtimp2,f01.d1por2,'% (52)',itimp2,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtimp3)
			insert into f99b values;
			(wjuri,'Actividad 3 : '+'(33) ',wtimp3,f01.d1por3,'% (53)',itimp3,.f.,.t.,.t.,.t.)
      endif

      if !empty(wtimp4)
			insert into f99b values;
			(wjuri,'Actividad 4 : '+'(34) ',wtimp4,f01.d1por4,'% (54)',itimp4,.f.,.t.,.t.,.t.)
      endif

      * Busqueda de Minimos y saldo a Favor
      * ------------------------------------

&& R.01b Ra๚l
*	   if !eof('f99c')
*	   	skip in f99c
*   	endif
&& R.01e Ra๚l

      sele f13
      
      if seek(ptoi(w5peri),'f13')
      	w5mini=f13.d13mini
      	w5rete=f13.d13rete
      	w5otpa=f13.d13otpa
      endif
      
**  	locate for val(right(w5peri,4)+left(w5peri,2))-1= ;
      				val(right(f13.d13peri,4)+left(f13.d13peri,2)) and ;
      				f13.d13juri=wjuri
     	locate for idp(w5peri,-1)=f13.d13peri and ;
      				f13.d13juri=wjuri
     	if found('f13')
     		w5safa=f13.d13safa
     	endif

		sele f99c
		
      if wtojui<w5mini
      	wtotre=w5mini
      else
      	wtotre=wtojui
      	w5mini=wtotre
      endif	

      wtores=wtotre- (w5rete+w5otpa) - w5safa

		insert into f99b values;
		(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,space(38)+'Subtotal............(56): ',0,0,'',wtojui,.f.,.f.,.f.,.t.)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)

		insert into f99b values;
		(wjuri,space(38)+'Imp. Alํcuota o Min.(57): ',0,0,'',w5mini,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Menos Retenciones...(59): ',0,0,'',w5rete,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Menos Otros Pagos...(60): ',0,0,'',w5otpa,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,space(38)+'Menos Saldo a Favor.(60): ',0,0,'',w5safa,.f.,.f.,.f.,.t.)
		insert into f99b values;
		(wjuri,'',0,0,'',0,.t.,.f.,.f.,.f.)
		insert into f99b values;
		(wjuri,space(38)+'Total...............(61):',0,0,'',wtores,.f.,.f.,.f.,.t.)

		insert into f99b values;
		(wjuri,'',0,0,'',0,.f.,.f.,.f.,.f.)


   endif

enddo

return

***puesto adrede para que incluya el report al proyecto**
report form iwr005m

