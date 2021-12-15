
function IfThenElse(condition,t,f) 
   if condition then
     return t
   else
     return f
   end 
end

function lengthovertime(time_sec)
   time = time_sec/spy
   if (time<timespan) then
       out = (time/timespan)*maxlength
   else
       out = maxlength*(1.0 - ((time - timespan)/timespan))
   end
   return out
end

function heightovertime(time_sec)
   time = time_sec/spy
   if (time<timespan) then
       out = time/(timespan)*maxheight
   else
       out =maxheight*(1.0 - ((time - timespan)/timespan))
   end
   return out
end



