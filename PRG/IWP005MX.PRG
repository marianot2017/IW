*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005M.PRG -> Liquidacion I.B. Alกcuota Fija.
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
use (d0f05) in 0 alias 'f05' order 'd5peri'			&&(Comprobantes)
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

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws005m.spr


pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf
dele file &d0f99.c.cdx

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons05m
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
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

set relation to 'JU'+d5juri  into f01
set relation to 'RV'+d5rubr  into f01t additive
set relation to ptoi(d5peri) into f13  additive

index on d5juri+f01t.d1tasa tag 'd5juri'

if !p_cancela
	return
endif

t_leye = 'Perํodo : '+w5peri+'   Nบ de Cuit : '+f00.p0cui+'  Nบ de Ing.Brutos : '+f00.p0nuib

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr005m'
	=rep_form()
	p_report=''
endif

set relation to
	

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

	wkperi = PtoI(w5peri)

   if ! seek(wkperi,'f04')
		if !w_show	   	
   	   wait window 'Perํodo Inexistente'
   	endif
     	w_retu=.f.
	endif
	
endif

return (w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC impu
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case
	case f01.d1tasa='1'
		wtojui=wtojui+round(wtimp*f01.d1por1/100,2)
	case f01.d1tasa='2'
		wtojui=wtojui+round(wtimp*f01.d1por2/100,2)
	case f01.d1tasa='3'
		wtojui=wtojui+round(wtimp*f01.d1por3/100,2)
	case f01.d1tasa='4'
		wtojui=wtojui+round(wtimp*f01.d1por4/100,2)
endcase
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC saldfav
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
sele f13
locate for val(ptoi(w5peri))-1 = val(ptoi(f13.d13peri)) and f01.d1cla=f13.d13juri
if found()
	w5safa=f13.d13safa
else
	w5safa=0
endif
sele f99c
return

***puesto adrede para que incluya el report al proyecto**
report form iwr005m

