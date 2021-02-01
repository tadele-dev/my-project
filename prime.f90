program isnprime
implicit none
integer::mod,starts,ends,n
print*,"enter value of N>=2"
read*,n
if(n<1)then
print*,n,"is not prime";stop
else
starts=2
ends = sqrt(n*1.0) +1
print*,"check ends at",ends
do while(starts < ends)
mod = n-(starts * (n/starts))
if (mod==0) then
print*,n,"is not prime"; stop
 end if
 starts = starts +1
 end do
 print*,n,"is a prime number"
 end if
 end program isnprime