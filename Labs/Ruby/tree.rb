class Tree
  attr_accessor :value, :left, :right
  def initialize(value, left=nil, right=nil)
    @value = value
    @left = left
    @right = right
  end

  def each_node(&blk)
    if !left.nil? 
      @left.each_node(&blk) 
    end
    if !right.nil? 
      @right.each_node(&blk) 
    end
    p value
  end

  def method_missing(m)
    oprs = m.to_s.split("_")
    if oprs[0] == "left"
      node = left
    else
      node = right
    end
   
    for i in 1..oprs.length do
      if oprs[i] == "left"
        node = node.left
      elsif
        oprs[i] == "right"
        node = node.right
      end
    end 
    node.value
  end

end

my_tree = Tree.new(42,
                   Tree.new(3,
                            Tree.new(1,
                                     Tree.new(7,
                                              Tree.new(22),
                                              Tree.new(123)),
                                     Tree.new(32))),
                   Tree.new(99,
                            Tree.new(81)))

my_tree.each_node do |v|
  puts v
end


arr = []
my_tree.each_node do |v|
  arr.push(v)
end
p arr

p "Getting nodes from tree"
p my_tree.left_left
p my_tree.right_left
p my_tree.left_left_right
p my_tree.left_left_left_right

