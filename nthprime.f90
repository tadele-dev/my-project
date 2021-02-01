program nthprime
implicit none
integer n,m,next,starts,ends,mode
logical ::found,isprime
m=0;next=2;print*,"enter the value of n>=1"
read*,n
if (n<1) then
print*,"invalid value of input";stop
else
do while(m<n)
            found=.false.
                  do while(.not. found)
                  starts = 2;ends=sqrt(next*1.0) +1;isprime=.true.
                            do while(starts<ends .and. isprime)
                               mode = next-(starts*(next/starts))
                               if(mode==0)isprime=.false.;starts=starts+1
                            end do
                               if(isprime)found=.true.; next=next+1
                       end do
                               m=m+1
    end do
    print*,"the nth prime number is ",next-1
end if
end program nthprime
                            