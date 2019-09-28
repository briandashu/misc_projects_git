#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################

#-----------------------------------------------------------
# FUNCTION DECLARATIONS
#-----------------------------------------------------------

def parse(file)
  puts "Not yet implemented"    
end

#part 1

def open_cells(file)
  line = file.gets

  if line == nil then return end
  
  count = 0

  while line = file.gets do
    x, y, ds, z = line.split(/\s/, 4)
    if ds.length == 4
      count = count + 1
    end
  end
  return count;
end



def bridge_num(file)
  line = file.gets
  if line == nil then return end

  count = 0

  while line = file.gets do
    x, y, ds, z = line.split(/\s/, 4)
    if ds =~ /d/ && ds =~ /u/
      count = count + 1
    end

    if ds =~ /l/ && ds =~ /r/
      count = count + 1
    end
  end
  return count
end

def cell_sort(file)
  line = file.gets

  zero = "0"
  one = "1"
  two = "2"
  three = "3"
  four = "4"

  while line = file.gets do
    x, y, ds, z = line.split(/\s/, 4)
    if(x =~ /\d/ && y =~ /\d/)
      if ds.length == 0
        zero.concat(",(" + x + "," + y +")")
      elsif ds.length == 1
        one.concat(",(" + x + "," + y +")")
      elsif ds.length == 2
        two.concat(",(" + x + "," + y +")")
      elsif ds.length == 3
        three.concat(",(" + x + "," + y +")")
      elsif ds.length == 4
        four.concat(",(" + x + "," + y +")")
      end
    end
  end

  finalRet = []
  finalRet << zero
  finalRet << one
  finalRet << two
  finalRet << three
  finalRet << four
  
  return finalRet

end

#part 2

def sort_by_cost(file)
  new_array = file_to_array(file)

  #i = 0
  #while new_array[i] != nil
  #  if new_array[i].is_path == false
  #    print new_array[i].get_x
  #    print " "
  #    print new_array[i].get_y
  #    print " "
  #    print new_array[i].get_udlr
  #    print " "
  #    print new_array[i].get_weight
  #    print " "
  #    print new_array[i].get_start
  #    print " "
  #    print new_array[i].get_ending
  #    print " "
  #    puts new_array[i].get_best
  #  elsif new_array[i].is_path == true
  #    print new_array[i].get_x
  #    print " "
  #    print new_array[i].get_y
  #    print " "
  #    print new_array[i].get_udlr
  #    print " "
  #    puts new_array[i].get_path_num
  #  end
  #  i = i + 1
  #end
  sum_i = 0
  sum_array = []
  i = 0
  returning_hash = Hash.new
  while new_array[i] != nil
    if new_array[i].is_path == true
      sum_array[sum_i] = sort_by_cost_helper(new_array, new_array[i].get_x, new_array[i].get_y, new_array[i].get_udlr).to_f
      returning_hash.store(sum_array[sum_i],new_array[i].get_path_num)
      sum_i = sum_i + 1
    end
    i = i + 1
  end
  paths = []
  temp = ""
  i = 0
  
  i = 0
  
  sum_array.sort!
  while sum_array[i] != nil
    if(sum_array[i] != 0.0)
      temp = sprintf("%10.4f %s", sum_array[i].to_f, returning_hash[sum_array[i]])
      # (sum_array[i].to_s + " " + returning_hash[sum_array[i]])
      paths << temp
    end
    i = i + 1
  end
  if paths.length == 0
    return "none"
  end
  return paths
end

def sort_by_cost_helper(maze_array, start_x, start_y, path) #return 1 path's total weight
  total_weight = 0
  maze_size = 0
  i = 0
  while(maze_array[i].is_path == false) # maze size
    maze_size = maze_size + 1
    i = i + 1
  end
  maze_size_squared = Math.sqrt(maze_size)
  
  path_size = path.length
  iterator = 0
  index = start_x.to_i * maze_size_squared.to_i + start_y.to_i #find starting index
  while index < (maze_size) && index >= 0 && iterator < path_size #while index not out and path size > 0
    if maze_array[index].get_udlr.include?path[0] #if index udlr matches a thing
      a, b, c, d = maze_array[index].get_weight.split(/\s/,4)
      if maze_array[index].get_udlr[0] == path[0] # first placing
        total_weight = total_weight + a.to_f
        if path[0] == "u"
          index = index - 1
        elsif path[0] == "d"
          index = index + 1
        elsif path[0] == "l"
          index = index - 4
        elsif path[0] == 'r'
          index = index + 4
        end
      elsif maze_array[index].get_udlr[1] == path[0] # second
        total_weight = total_weight + b.to_f
        if path[0] == "u"
          index = index - 1
        elsif path[0] == "d"
          index = index + 1
        elsif path[0] == "l"
          index = index - 4
        elsif path[0] == 'r'
          index = index + 4
        end
        
      elsif maze_array[index].get_udlr[2] == path[0] # third 
        total_weight = total_weight + c.to_f
        if path[0] == "u"
          index = index - 1
        elsif path[0] == "d"
          index = index + 1
        elsif path[0] == "l"
          index = index - 4
        elsif path[0] == 'r'
          index = index + 4
        end
       
      elsif maze_array[index].get_udlr[3] == path[0] #fourth 
        total_weight = total_weight + d.to_f
        if path[0] == "u"
          index = index - 1
        elsif path[0] == "d"
          index = index + 1
        elsif path[0] == "l"
          index = index - 4
        elsif path[0] == 'r'
          index = index + 4
        end
      end
      path = path[1...path.length]
    elsif iterator < path_size - 1
      return "none"
    end
    iterator = iterator + 1
  end
  return total_weight
end

class Filepath
  def initialize(x_start, y_start, udlr, weight, path_num)
    @x_start = x_start
    @y_start = y_start
    @udlr = udlr
    @weight = weight
    @path_num = path_num
  end
  def get_x
    return @x_start
  end
  def get_y
    return @y_start
  end
  def get_udlr
    return @udlr
  end
  def get_weight
    return @weight
  end
  def set_weight(new_weight)
    @weight = new_weight
  end
  def get_path_num
    return @path_num
  end
  def is_path
    return true
  end
end

class Node
  def initialize(x_value, y_value, udlr, weight, start, ending, best)
    @x_value = x_value
    @y_value = y_value
    @udlr = udlr
    @weight = weight
    @start = start
    @ending = ending
    @best = best #in the best path
  end
  def get_x
    return @x_value
  end
  def get_y
    return @y_value
  end
  def get_udlr
    return @udlr
  end
  def get_weight
    return @weight
  end 
  def get_start
    return @start
  end
  def get_ending
    return @ending
  end
  def get_best
    return @best
  end
  def is_start
    @start = true
  end
  def is_ending
    @ending = true
  end
  def is_path
    return false
  end
  def is_best
    @best = true
  end
  def get_index(size)
    return @x.to_i * size + @y.to_i
  end
end

def file_to_array(file)
  line = file.gets
  start_x = 0
  start_y = 0
  ending_x = 0
  ending_y = 0

  n, x, y, xx, yy = line.split(/\s/)
  start_x = x.to_i
  start_y = y.to_i
  ending_x = xx.to_i
  ending_y = yy.to_i

  #print start_x
  #print start_y
  #print ending_x
  #puts ending_y

  new_array =[]
  i = 0;
  path_i = 1;
  while line = file.gets do
    if line[0...4] != "path"
      x, y, ds, z = line.split(/\s/, 4)
      new_node = Node.new(x, y, ds, z, false, false, false)
      new_array[i] = new_node
      i = i + 1
    elsif line[0...4] == "path"
      a, b, x, y, z = line.split(/\s/, 5)
      new_path = Filepath.new(x, y, z, 0, b)
      new_array[i] = new_path
      path_i = path_i + 1
      i = i + 1
    end
  end
  
  i = 0

  while new_array[i] != nil
    if(new_array[i].is_path == false)
      if new_array[i].get_x.to_i == start_x && new_array[i].get_y.to_i == start_y
        new_array[i].is_start
      end
      if new_array[i].get_x.to_i == ending_x && new_array[i].get_y.to_i == ending_y
        new_array[i].is_ending
      end
    end
    i = i + 1
  end
  # i = 0
  # while new_array[i] != nil
    #puts new_array[i].is_path
    #i = i + 1
  #end
   
  i = 0
  count = 0
  #while new_array[i] != nil
    #print new_array[i].get_x
    #print " "
    #print new_array[i].get_y
    #print " "
    #print new_array[i].get_udlr
    #print " "
    #print new_array[i].get_weight
    #print " "
    #print new_array[i].get_start
    #print " "
    #print new_array[i].get_ending
    #print " "
    #puts new_array[i].get_best
  

    #i = i + 1
  #end
  
  return new_array
  
  
end

#part 3


def pretty_print(file)
  maze_array = file_to_array(file)
  path_array = []
  path_array_x = []
  path_array_y = []
  path_value_array = []
  
  i = 0
  while maze_array[i] != nil
    if maze_array[i].is_path == true
      path_array << maze_array[i].get_udlr
      path_array_x << maze_array[i].get_x
      path_array_y << maze_array[i].get_y
    end
    i = i + 1
  end
  i = 0
  while i < path_array.size
     path_value_array << sort_by_cost_helper(maze_array, path_array_x[i], path_array_y[i], path_array[i])
    i = i + 1
  end
  i = 0
  lowest = 1000000000
  least_path = ""
  least_path_x = 0
  least_path_y = 0
  while path_value_array[i] != nil
    if path_value_array[i] != "none"
      if path_value_array[i] < lowest
        lowest = path_value_array[i]
        least_path = path_array[i]
        least_path_x = path_array_x[i]
        least_path_y = path_array_y[i]
      end
    end
    i = i + 1
  end

   # least_path is the path, least_path_x is the x start, least_path_y is the y start, lowest is the path
  #puts least_path
  #puts least_path_x
  #puts least_path_y
  #puts lowest
  #maze_array is the maze in array form
  maze_size = 0
  i = 0
  
  while maze_array[i] != nil && maze_array[i].is_path == false
    if maze_array[i].is_path == false
      maze_size = maze_size + 1
      i = i + 1
    end
  end
  maze_size = Math.sqrt(maze_size)
  maze_size = maze_size.to_i
  
  
  index = least_path_x.to_i * maze_size + least_path_y.to_i
  

  while least_path[0] != nil
    maze_array[index].is_best == true
    if least_path[0] == "u"
      index = index - 1
    elsif least_path[0] == "d"
      index = index + 1
    elsif least_path[0] == "l"
      index = index - 4
    elsif least_path[0] == "r"
      index = index + 4
    end
    least_path = least_path[1...least_path.length]
  end

  #i = 0
  #count = 0
  #while i < 16
  #  print maze_array[i].get_x
  #  print " "
  #  print maze_array[i].get_y
  #  print " "
  #  print maze_array[i].get_udlr
  #  print " "
  #  print maze_array[i].get_weight
  #  print " "
  #  print maze_array[i].get_start
  #  print " "
  #  print maze_array[i].get_ending
  #  print " "
  #  puts maze_array[i].get_best
  #   i = i + 1
  #end

  maze_printout = ""
  i = 0
  maze_printout.concat("+")
  while i < maze_size
    maze_printout.concat("-+")
    i = i + 1
  end
  maze_printout.concat("\n")
  #prints first line and left wall
  #puts maze_size maze size = 4 if div by 4 \n

  index = 0
  i = 0
  is_end = false

  temp_top_print = "|"
  temp_bottom_print = "+"

  while i < (maze_size * maze_size) + (maze_size - 1)
    if index > ((maze_size * maze_size) - 1)
      index = index - (maze_size * maze_size) + 1
      maze_printout.concat(temp_top_print)
      maze_printout.concat("\n")
      maze_printout.concat(temp_bottom_print)
      maze_printout.concat("\n")
      temp_top_print = "|"
      temp_bottom_print = "+"
    end
    if maze_array[index].get_start == false && maze_array[index].get_ending == false && maze_array[index].get_best == true
      temp_top_print.concat("*")
      if maze_array[index].get_udlr.include?"r"
        temp_top_print.concat(" ")
      else
        temp_top_print.concat("|")
      end

      if maze_array[index].get_udlr.include?"d"
        temp_bottom_print.concat(" ")
        temp_bottom_print.concat("+")
      else
        temp_bottom_print.concat("-")
        temp_bottom_print.concat("+")
      end
    elsif maze_array[index].get_start == true && maze_array[index].get_ending == false && maze_array[index].get_best == true
      temp_top_print.concat("S")
      if maze_array[index].get_udlr.include?"r"
        temp_top_print.concat(" ")
      else
        temp_top_print.concat("|")
      end

      if maze_array[index].get_udlr.include?"d"
        temp_bottom_print.concat(" ")
        temp_bottom_print.concat("+")
      else
        temp_bottom_print.concat("-")
        temp_bottom_print.concat("+")
      end
    elsif maze_array[index].get_start == true && maze_array[index].get_ending == false && maze_array[index].get_best == false
      temp_top_print.concat("s")
      if maze_array[index].get_udlr.include?"r"
        temp_top_print.concat(" ")
      else
        temp_top_print.concat("|")
      end

      if maze_array[index].get_udlr.include?"d"
        temp_bottom_print.concat(" ")
        temp_bottom_print.concat("+")
      else
        temp_bottom_print.concat("-")
        temp_bottom_print.concat("+")
      end
    elsif maze_array[index].get_start == false && maze_array[index].get_ending == true && maze_array[index].get_best == true
      temp_top_print.concat("E")
      if maze_array[index].get_udlr.include?"r"
        temp_top_print.concat(" ")
      else
        temp_top_print.concat("|")
      end

      if maze_array[index].get_udlr.include?"d"
        temp_bottom_print.concat(" ")
        temp_bottom_print.concat("+")
      else
        temp_bottom_print.concat("-")
        temp_bottom_print.concat("+")
      end
    elsif maze_array[index].get_start == false && maze_array[index].get_ending == true && maze_array[index].get_best == false
      temp_top_print.concat("e")
      if maze_array[index].get_udlr.include?"r"
        temp_top_print.concat(" ")
      else
        temp_top_print.concat("|")
      end

      if maze_array[index].get_udlr.include?"d"
        temp_bottom_print.concat(" ")
        temp_bottom_print.concat("+")
      else
        temp_bottom_print.concat("-")
        temp_bottom_print.concat("+")
      end
    else
      temp_top_print.concat(" ")
      if maze_array[index].get_udlr.include?"r"
        temp_top_print.concat(" ")
      else
        temp_top_print.concat("|")
      end

      if maze_array[index].get_udlr.include?"d"
        temp_bottom_print.concat(" ")
        temp_bottom_print.concat("+")
      else
        temp_bottom_print.concat("-")
        temp_bottom_print.concat("+")
      end
    end
    index = index + maze_size
    i = i + 1
    
  end
  return maze_printout[0...maze_printout.length - 1]

end

#part 4

def distance_of_cells(file)
  maze_array = file_to_array(file)
  maze_size  = 0
  start_x = 0
  start_y = 0
  i = 0
  
  while maze_array[i] != nil && maze_array[i].is_path == false
    if maze_array[i].get_start == true
      start_x = maze_array[i].get_x
      start_y = maze_array[i].get_y
    end
    if maze_array[i].is_path == false
      maze_size = maze_size + 1
      i = i + 1
    end
  end
  #puts start_x
  #puts start_y
  #maze_size = the total spaces
  maze_size_squared = Math.sqrt(maze_size).to_i
  
  
  index = start_x.to_i * maze_size_squared + start_y.to_i #index of start
  
  temp_array = []
  temp_array_2 = []
  temp_array << maze_array[index]
  tier = 0
  contained = []
  #puts maze_array[2].get_udlr.include? "r"

  return_string  = ""

  #------loop----------------
  while temp_array.length > 0
    #print "tier "
    #puts tier
    i = 0
    temp_array.uniq!
    temp_array.sort! {|a, b| a.get_x.to_i * maze_size_squared + a.get_y.to_i <=> b.get_x.to_i * maze_size_squared + b.get_y.to_i}
    while temp_array[i] != nil
      #print temp_array[i].get_udlr
      contained << temp_array[i]
      i = i + 1
      #print " "
    end
    #puts " "
    #puts "+++++++"
    i = 0
    return_string.concat(tier.to_s)
    while temp_array[i] != nil
      return_string.concat(",")
      return_string.concat("(")
      return_string.concat(temp_array[i].get_x)
      return_string.concat(",")
      return_string.concat(temp_array[i].get_y)
      return_string.concat(")")
      i = i + 1
    end
    return_string.concat("\n")
    i = 0
    while temp_array[i] != nil
      if temp_array[i].get_udlr.include? "u"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        if contained.include?maze_array[index2 - 1]
        else
          temp_array_2 << maze_array[index2 - 1]
          #puts "went up"
        end
    
      end
       if temp_array[i].get_udlr.include? "d"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        
        if contained.include?maze_array[index2 + 1]
        else
          temp_array_2 << maze_array[index2 + 1]
          #puts "went down"
        end
        
      end
      if temp_array[i].get_udlr.include? "l"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        
        if contained.include?maze_array[index2 - 4]
        else
          temp_array_2 << maze_array[index2 - 4]
          #puts "went left"
        end

      end
      if temp_array[i].get_udlr.include? "r"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        if contained.include?maze_array[index2 + 4]
        else
          temp_array_2 << maze_array[index2 + 4]
          #puts "went right"
        end
        
      end
      i = i + 1
    end
    
    i = 0
    temp_array.clear
    while temp_array_2[i] != nil
      temp_array << temp_array_2[i]
      i = i + 1
    end
    temp_array_2.clear
    tier  = tier + 1
    
  end

  return return_string[0...return_string.length - 1]
end

#part 5

def solve_maze(file)
  maze_array = file_to_array(file)
  maze_size  = 0
  start_x = 0
  start_y = 0
  i = 0
  
  while maze_array[i] != nil && maze_array[i].is_path == false
    if maze_array[i].get_start == true
      start_x = maze_array[i].get_x
      start_y = maze_array[i].get_y
    end
    if maze_array[i].is_path == false
      maze_size = maze_size + 1
      i = i + 1
    end
  end
  #puts start_x
  #puts start_y
  #maze_size = the total spaces
  maze_size_squared = Math.sqrt(maze_size).to_i
  
  
  index = start_x.to_i * maze_size_squared + start_y.to_i #index of start
  
  temp_array = []
  temp_array_2 = []
  temp_array << maze_array[index]
  tier = 0
  contained = []
  #puts maze_array[2].get_udlr.include? "r"

  return_string  = ""

  #------loop----------------
  while temp_array.length > 0
    #print "tier "
    #puts tier
    i = 0
    temp_array.uniq!
    temp_array.sort! {|a, b| a.get_x.to_i * maze_size_squared + a.get_y.to_i <=> b.get_x.to_i * maze_size_squared + b.get_y.to_i}
    while temp_array[i] != nil
      #print temp_array[i].get_udlr
      contained << temp_array[i]
      i = i + 1
      #print " "
    end
    #puts " "
    #puts "+++++++"
    i = 0
    return_string.concat(tier.to_s)
    while temp_array[i] != nil
      return_string.concat(",")
      return_string.concat("(")
      return_string.concat(temp_array[i].get_x)
      return_string.concat(",")
      return_string.concat(temp_array[i].get_y)
      return_string.concat(")")
      i = i + 1
    end
    return_string.concat("\n")
    i = 0
    while temp_array[i] != nil
      if temp_array[i].get_udlr.include? "u"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        if contained.include?maze_array[index2 - 1]
        else
          temp_array_2 << maze_array[index2 - 1]
          #puts "went up"
        end
    
      end
       if temp_array[i].get_udlr.include? "d"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        
        if contained.include?maze_array[index2 + 1]
        else
          temp_array_2 << maze_array[index2 + 1]
          #puts "went down"
        end
        
      end
      if temp_array[i].get_udlr.include? "l"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        
        if contained.include?maze_array[index2 - 4]
        else
          temp_array_2 << maze_array[index2 - 4]
          #puts "went left"
        end

      end
      if temp_array[i].get_udlr.include? "r"
        index2 = temp_array[i].get_x.to_i * maze_size_squared + temp_array[i].get_y.to_i
        if contained.include?maze_array[index2 + 4]
        else
          temp_array_2 << maze_array[index2 + 4]
          #puts "went right"
        end
        
      end
      i = i + 1
    end
    
    i = 0
    temp_array.clear
    while temp_array_2[i] != nil
      temp_array << temp_array_2[i]
      i = i + 1
    end
    temp_array_2.clear
    tier  = tier + 1
    
  end
  
  is_solved = false
  final_destination = ""
  i = 0
  
  while maze_array[i] != nil && maze_array[i].is_path == false
    if maze_array[i].get_ending == true
      final_destination.concat(maze_array[i].get_x)
      final_destination.concat(",")
      final_destination.concat(maze_array[i].get_y)
    end
    i = i + 1
  end
  #puts final_destination

  if return_string.include? final_destination
    is_solved = true
  end
  return is_solved
end

#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify 
# this function however you like in working on your assignment.

def read_and_print_simple_file(file)
  line = file.gets
  if line == nil then return end

  # read 1st line, must be maze header
  sz, sx, sy, ex, ey = line.split(/\s/)
  puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})"

  # read additional lines
  while line = file.gets do

    # begins with "path", must be path specification
    if line[0...4] == "path"
      p, name, x, y, ds = line.split(/\s/)
      puts "path spec: #{name} starts at (#{x},#{y}) with dirs #{ds}"

    # otherwise must be cell specification (since maze spec must be valid)
    else
      x, y, ds, w = line.split(/\s/,4)
      puts "cell spec: coordinates (#{x},#{y}) with dirs #{ds}"
      ws = w.split(/\s/)
      ws.each {|w| puts "  weight #{w}"}
    end
  end
end

#----------------------------------
def main(command_name, file_name)
  maze_file = open(file_name)

  # perform command
  case command_name
  when "open" 
    open_cells(maze_file)
  when "bridge"
    bridge_num(maze_file)
  when "sortcells"
    cell_sort(maze_file)
  when "paths"
    sort_by_cost(maze_file)
  when "parse"
    parse(maze_file)
  when "print"
    pretty_print(maze_file)
  when "distance"
    distance_of_cells(maze_file)
  when "solve"
    solve_maze(maze_file)
  else
    fail "Invalid command"
  end
end
