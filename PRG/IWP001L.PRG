*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP001L.PRG -> Listador de tablas
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

w_pfile='01l'
w_prog =upper(prompt())
w_title=upper(prompt())
w_alta =.f.
w_imco=imco()

*) Variables-------------------------------------------------------------------

w_pict=iif(w_pref='JU','@K! X','999999')
w_dcla=iif(w_pref='JU','  ',space(6))
w_hcla=iif(w_pref='JU','Z ','999999')

w_drep=''
w_hrep=''

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F01'
fieldvec[2]='F02'
fieldvec[3]='F13'
fieldvec[4]='F23'

use (d0f00) in 0 alias 'f00'						&&(Parametros generales)
use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
use (d0f13) in 0 alias 'f13' order 'd13peri' &&(Cat.Impositivas)
use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

if w_imco
	ncf01=w0fipre+'01'
   fieldvec[4]='F01c'
   do while .t.
   	use (ncf01) in 0 alias 'f01c' order 'd1cuen'
   	if !neterr()
   		exit
   	endif
   enddo
   fieldvec[4]=''
endif

if neterr()		
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

if w_pref='RV'
	w_tocv1=f00.p0ocv1
	w_tocv2=f00.p0ocv2
	w_tocv3=f00.p0ocv3
else
	w_tocv1=f00.p0occ1
	w_tocv2=f00.p0occ2
	w_tocv3=f00.p0occ3
endif

do ssm998.mpr
do iws001l.spr

pop key  
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons01l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons01l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons01l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid01l(.t.)
	wait wind 'Verifique los datos ingresados...'
   return 
endif

=gentmp()

w_drep=w_dcla
w_hrep=w_hcla

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
select *;
	from f01;
	where d1tip=w_pref and between(d1cla,w_dcla,w_hcla);
	into dbf &d0f99.c order by d1tip,d1cla
use &d0f99.c alias f99c exclusive

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top

if !empty(p_repclaus) &&(debe ser preview o to printer)
	if w_pref$'RC.RV.RS.RP'
		p_report='iwr001l'
	else
		p_report='iwr001lj'
	endif
	
	=rep_form()
	
	p_report=''
endif

do represet &&(Resetea estado)

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid01l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_DCLA' or w_show
	if w_pref$'RC.RV'
		w_dcla=strcero(w_dcla)
		show get w_dcla
	endif
	if lastk()=p_f4
		do fhelp with 'W_DCLA',(w_pref),'iwp001','f01','d1cla','d1cla','',(p_char),'w_retu'
	endif
endif

if w_vari='W_HCLA' or w_show
	if w_pref$'RC.RV'
		w_hcla=strcero(w_hcla)
		show get w_hcla
	endif
	if lastk()=p_f4
		do fhelp with 'W_HCLA',(w_pref),'iwp001','f01','d1cla','d1cla','',(p_char),'w_retu'
	endif
	if w_dcla>w_hcla
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC cuen01l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_cuen
return(iif(used('f01c'),iif(seek(w_cuen,'f01c'),f01c.d1desc,''),''))

***puesto adrede para que incluya el report al proyecto**
report form iwr001l
report form iwr001lj

