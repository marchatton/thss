print(lw_)
print(uw_)
print(psc_SPvol[iii,jjj])
print(dv_SPinitial[jjj])
print(SPvar)

print(delv_var.future)
print(SPvar.future)


print((psc_SPvol[iii,jjj] + delv_var.future < lw_))
print(delv_emer.future[iii+1,jjj])
print((dv_delv[iii+1,jjj] + delv_emer.future[iii+1,jjj] > ulim_delv[jjj]))
print(delv_emer.future[iii+1,jjj])

print((psc_SPvol[iii,jjj] + delv_var.future) > uw_ )
print(delv_canc.future[iii+1,jjj])
print((dv_delv[iii+1,jjj] - delv_canc.future[iii+1,jjj] < llim_delv[jjj]))
print(delv_canc.future[iii+1,jjj])

print(leadtime)
delv_emer.future[,jjj]
delv_emer[,jjj]
delv_canc.future[,jjj]
delv_canc[,jjj]

