class Array
  # ['a', 'b'].extract_if_singleton => ['a', 'b']
  # ['b'].extract_if_singleton      => 'b'
  def extract_if_singleton
    (length==1) ? first : self
  end
end

class Hash
  def safe_merge(new_hash)
    self.merge(new_hash){|key,old,new|
      if new.nil? || (new.respond_to?(:empty?) && new.empty?)
        old
      else
        new
      end
    }
  end
end
