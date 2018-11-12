*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : cgp001cl.prg -> Listado de Cuentas
*)-H
*)-H Fecha de inicio : 
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
EXTERNAL ARRAY _PDPARMS
PRIVATE all like w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_pfile='01cl'
w_prog=upper(prompt())

*) Variables-------------------------------------------------------------------

w_alias='f01c'
w_dbname=db_name(w_alias)
w_alta=.f.

w_dcuen=space(20)
w_hcuen=replicate('9',20)
w_comp ='S'
w_dsuon=''
w_dsuof=''
w_ddato=''

*) Apertura de archivos--------------------------------------------------------

ncf00=w0fipre+'00'
ncf01=w0fipre+'01'

fieldvec[1]='F00c'
fieldvec[2]='F01c'

use (ncf00) in 0 alias 'f00c' 				 &&(Parametros)
use (ncf01) in 0 alias 'f01c' order d1cuen &&(Cuentas)

if neterr()
	pop key  
   pop menu _msysmenu
   fieldvec=''
   =closall()
   return
endif

*)-----------------------------------------------------------------------------

do ssm998.mpr &&(Menu Ayuda)
do cgs001cl.spr

pop key
pop menu _msysmenu
=closall()
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Cons01cl
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid01cl(.t.)
   return 
endif

do repdest &&(Setea variables internas del reporte)

sele (w_alias)
go top
if !empty(p_repclaus)	&&(debe ser preview o to printer)

	p_report='cgr001cl'
	=resacta(p_report, d0f99) &&(solo toca el compactado)
	sele (w_alias)
	p_report=d0f99	
	p_repfor='ptoc(w_dcuen) <= f01c.d1cuen and f01c.d1cuen <= ptoc(w_hcuen)'
	=rep_form()
	p_report=''
	
else &&(to file)

	p_repfields='d1cuen, d1desc'
	p_repfor='ptoc(w_dcuen) <= f01c.d1cuen and f01c.d1cuen <= ptoc(w_hcuen)'
	p_repwhile='p_cancela'
	
endif

do represet &&(Resetea estado)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC resacta
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_base, w_d0f99
private all w_*

w_base='report\'+w_base+'.frx'

use &w_base in 0 alias freport         && Reporte

sele freport
p_width=width
p_report=dbf()
use        
p_congtit=.t.

use &p_report in 0 alias frpt
sele frpt

set safety off

copy to &w_d0f99..frx
use
use &w_d0f99..frx in 0 alias frpt

replace fontstyle with 5 for platform='WINDOWS' and "d1resa='S'"$supexpr
use
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Valid01cl
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show
PRIVATE w_retu,w_vari

w_retu=.t.
w_vari=varread()

if lastkey()=p_Esc
    return .t.
endif

if w_vari='W_DCUEN' .or. w_show

    if lastkey()=p_Up
        return .t.
    endif
    if lastkey()=p_F4
        w_dcuen=ptoc(w_dcuen)
        do fhelp with 'W_DCUEN','','cgp001c','f01c','d1cuen' ,'d1desc', 'Cuenta inexistente',(p_char),'w_retu'
        w_dcuen=ctop(f01c->d1cuen)
        show get w_dcuen
    endif

endif

if w_vari='W_HCUEN' .or. w_show

    if empty(ptoc(w_hcuen))
        w_hcuen='999999999999999'
    endif
    show get w_hcuen
    if lastkey()=p_F4
        w_hcuen=ptoc(w_hcuen)
        do fhelp with 'W_HCUEN','','cgp001c','f01c','d1cuen' ,'d1desc', 'Cuenta inexistente',(p_char),'w_retu'
        w_hcuen=ctop(f01c->d1cuen)
        show get w_hcuen
   endif
   if w_dcuen>w_hcuen
   	wait 'Hasta debe ser mayor o igual a desde' wind nowai
   	return .f.
	endif

endif
return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Subr01cl
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_stri=substr(replicate(' ',(nivel(d1cuen)-1)*2)+f01c.d1desc+space(40),1,46)+'  '+f01c.d1ptrs+;
                    '  '+str(f01c.d1colu,2)+'  '+f01c.d1salt+'  '+str(f01c.d1inte,2)+;
                    '  '+f01c.d1resa+'  '+f01c.d1leye+'  '+f01c.d1indi+;
                    '  '+f05.d5desc
if .not. p_repout='Pantalla'
   if p_repout='Impresora Laser' .or. p_repout='Impresara HP' .or.;
      p_repout='Impresora DeskJet'
         _pdparms[15]=_pdparms[11]
         _pdparms[16]=_pdparms[12]
   else
         _pdparms[15]=chr(27)+'G'
         _pdparms[16]=chr(27)+'H'            
   endif
   if f01c.d1resa='S'
       wdsuon=_pdparms[15]+_pdparms[13]
       wdsuof=_pdparms[16]+_pdparms[14]
       w_space=len(_pdparms[13])+len(_pdparms[14])+len(_pdparms[15])+len(_pdparms[16])
       wddato=substr(replicate(' ',(nivel(d1cuen)-1)*2)+wdsuon+rtrim(f01c.d1desc)+wdsuof+space(50),1,46+w_space)+;
                              '  '+f01c.d1ptrs+;
                              '  '+str(f01c.d1colu,2)+'  '+f01c.d1salt+'  '+str(f01c.d1inte,2)+;
                              '  '+f01c.d1resa+'  '+f01c.d1leye+'  '+f01c.d1indi+;
                              '  '+f05.d5desc
   else
       wddato=w_stri
       wdsuon=''
       wdsuof=''
   endif
else
   wddato=w_stri
endif
return .t.

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC xxCons01cl
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if .not. valid01cl(.t.)
   return 
endif

if w_comp='N'
    use report\cgr001cl2.frx in 0 alias freport         && Reporte
else
    use report\cgr001cl.frx in 0 alias freport          && Reporte
endif

sele freport
p_width=width
p_report=dbf()
use        
p_congtit=.t.

sele (w_alias)

do repdest  && Setea variables internas del reporte

sele (w_alias)
=seek(ptoc(wdcuen),'f01c')

p_repwhile='f01c.d1cuen <= ptoc(whcuen) and subr01cl()'
p_report=&d0f99

=rep_form()
p_report=''

do represet && Resetea estado 
return
