# -*- coding: UTF-8 -*-
module SimplerNLG

  # class Realiser
  #   def realize_sentence(i)
  #     realise_sentence(i)
  #   end
  # end

  class NLG
    MODIFIERS = [:add_post_modifier, :add_pre_modifier, :add_front_modifier]

    # use module's imported packages
    def self.const_missing const ; SimplerNLG.const_missing const ; end

    # optional value access helper
    def self.with value, &block ; block.call(value) if value ; end

    # use class methods directly in instance context blocks
    def method_missing name, *args, &block
      return self.class.send name, *args, &block if self.class.respond_to? name
      super.method_missing name, *args, &block
    end

    # setup basic components
    @@lexicon = XMLLexicon.new File.expand_path("../../res/default-lexicon.xml", __FILE__)
    @@factory = NLGFactory.new @@lexicon
    @@realiser = Realiser.new @@lexicon
    # @@realiser.debug_mode = true

    def self.factory
      @@factory
    end
    def self.realiser
      @@realiser
    end

    def self.realizer #,geez, the brits!
      @@realiser
    end

    # static hash accessor for convenience
    def self.[] *args, &block ;  self.render *args, &block ; end

    # main method
    def self.render *args, &block  
      return @@realiser.realise_sentence @@factory.create_sentence new.instance_eval &block if block
      input = args ? args.first : nil
      return "" unless input
      return @@realiser.realise_sentence(input.is_a?(String) ? @@factory.create_sentence(input) : phrase(input))
    end

    def self.phrase input

      clause = @@factory.create_clause
      
      # SVO (init)
      s = input[:subject] || input[:s] || ""
      v = input[:verb]    || input[:v]
      o = input[:object]  || input[:o]
      svo = {:s=>s, :v=>v, :o=>o}

      # SVO (conjunction and modifier)
      svo.each do |type, arg|
        svo[type] = mod_helper type, arg, input if arg.is_a?(Container) && arg.is(:modifier)
        if arg.is_a?(Array)
          conjunction_type = arg.shift if [:and,:or, :nor, :neither_nor].include? arg.first
          if arg.length >= 2
            modded_args = arg.map{|part| part.is_a?(Container) && part.is(:modifier) ? mod_helper(type, part, input) : part }
            svo[type] = @@factory.create_coordinated_phrase *modded_args[0..1]
            modded_args.drop(2).each{ |additional_coordinate| svo[type].add_coordinate(additional_coordinate) }
          end
          svo[type].set_feature Feature::CONJUNCTION, conjunction_type if conjunction_type
        elsif arg.is_a?(Hash) and [:s, :o].include?(type)
          svo[type] = self.noun_phrase_helper(arg)
        end
      end

      # SVO (finalize)
      clause.subject = svo[:s]
      clause.verb    = svo[:v] if svo[:v] 
      clause.object  = svo[:o] if svo[:o] 

      # FEATURES:
      with input[:n] || input[:negation] || input[:negated] do |value|
        clause.set_feature Feature::NEGATED, value
      end
      with input[:passive] do |value|
        clause.set_feature Feature::PASSIVE, value
      end
      with input[:q] || input[:question] || input[:interrogation] do |type|
        @@interrogative_types ||= Hash[:yes_no               => InterrogativeType::YES_NO,
                                       :binary               => InterrogativeType::YES_NO,
                                       :who                  => InterrogativeType::WHO_SUBJECT,
                                       :who_s                => InterrogativeType::WHO_SUBJECT,
                                       :who_subject          => InterrogativeType::WHO_SUBJECT,
                                       :who_o                => InterrogativeType::WHO_OBJECT,
                                       :who_object           => InterrogativeType::WHO_OBJECT,
                                       :who_indirect_object  => InterrogativeType::WHO_INDIRECT_OBJECT,
                                       :where                => InterrogativeType::WHERE,
                                       :how                  => InterrogativeType::HOW,
                                       :what                 => InterrogativeType::WHAT_SUBJECT,
                                       :what_s               => InterrogativeType::WHAT_SUBJECT,
                                       :what_subject         => InterrogativeType::WHAT_SUBJECT,
                                       :what_o               => InterrogativeType::WHAT_OBJECT,
                                       :what_object          => InterrogativeType::WHAT_OBJECT,
                                       :why                  => InterrogativeType::WHY,
                                       :how_many             => InterrogativeType::HOW_MANY]
        clause.set_feature Feature::INTERROGATIVE_TYPE, @@interrogative_types[type.to_sym]
      end
      with input[:t] || input[:tense] do |tense|
        @@tenses ||= Hash[:present => Tense::PRESENT, :past => Tense::PAST, :future => Tense::FUTURE]
        clause.set_feature Feature::TENSE, @@tenses[tense.to_sym]
      end
      with input[:nr] || input[:number] do |number|
        number = number == 1 ? :singular : :plural if number.is_a? Fixnum
        @@number ||= Hash[:singular => NumberAgreement::SINGULAR, :plural => NumberAgreement::PLURAL, :both => NumberAgreement::BOTH]
        clause.set_feature Feature::NUMBER, @@number[number.to_sym]
      end
      with input[:perfect] do |perfect|
        clause.set_feature Feature::PERFECT, perfect
      end
      with input[:progressive] do |progressive|
        clause.set_feature Feature::PROGRESSIVE, progressive
      end
      # with input[:prepositional_phrase] || input[:pp] do |pp|
      #   clause
      # end
      with input[:modal] do |modal|
        clause.set_feature Feature::MODAL, modal
      end

      #ALSO: ADJECTIVE_ORDERING AGGREGATE_AUXILIARY APPOSITIVE CUE_PHRASE FORM IS_COMPARATIVE IS_SUPERLATIVE PATTERN PARTICLE PERFECT PERSON POSSESSIVE PRONOMINAL RAISE_SPECIFIER SUPPRESS_GENITIVE_IN_GERUND SUPRESSED_COMPLEMENTISER
      #ELIDED is pretty useless (if not dangerous! - NullPointerException in OrthographyProcessor.removePunctSpace())

      # COMPLEMENT
      [input[:complement],input[:complements],input[:c]].flatten(1).each do |complement|
        if false # && complement.is_a?(Clause)

          # ["in any year since 1992 when Atlantic tropical storm-related deaths ended in an odd number", "except 1996"] )


          # np = @@factory.create_noun_phrase("1996"); # was nlgFactory, of type NPPhraseSpec
          # pp = @@factory.createPrepositionPhrase(); # of type PPPhraseSpec 
          # pp.add_complement(np);
          # pp.set_preposition("since");
          # clause.add_complement pp

          # np_place = @@factory.create_noun_phrase("the", "park"); # was nlgFactory, of type NPPhraseSpec
          # pp_place = @@factory.createPrepositionPhrase(); # of type PPPhraseSpec 
          # pp_place.add_complement(np_place);
          # pp_place.set_preposition("in");

          clause.add_complement pp_place
        else
          clause.add_complement complement.to_s # to_s added for method signature reasons...
        end
      end

      [input[:prepositional_phrases], input[:prepositional_phrase], input[:pp]].flatten(1) do |pp|
        if pp[:position] == :front
          clause.add_front_modifier(pp)
        elsif pp[:position] == :pre
          clause.add_pre_modifier(pp)
        elsif pp[:position] == :post
          clause.add_post_modifier(pp)
        else
          clause.add_modifier(pp)
        end
      end

      return clause

    end

    def self.mod_helper type, container, input
      core = container.content.first
      mod_phrase = self.noun_phrase_helper core if [:s, :o].include? type
      mod_phrase = @@factory.create_verb_phrase core if type == :v
      container.content.drop(1).each do |modifier|
        case container.sub_type
          when :front     then mod_phrase.add_front_modifier modifier.to_s
          when :pre       then mod_phrase.add_pre_modifier   modifier.to_s
          when :post      then mod_phrase.add_post_modifier  modifier.to_s
          # when :anymod    then mod.send(([:add_front_modifier, :add_pre_modifier, :add_post_modifier] - :TODO_ACTUAL_THING_TO_REMOVE ).sample, modifier.to_s)
          when :adjective then begin
            adjective = @@factory.create_adjective_phrase(modifier.to_s)
            with input[:comparative] || input[:comp] do |comparative|
              adjective.set_feature Feature::IS_COMPARATIVE, comparative
            end
            with input[:superlative] || input[:super] do |superlative|
              adjective.set_feature Feature::IS_SUPERLATIVE, superlative
            end
            mod_phrase.add_modifier(adjective)  
          end
          else mod_phrase.add_modifier modifier.to_s
        end
      end
      return mod_phrase
    end

    def self.mod *args ; return Container.new :modifier, [args].flatten ; end
    def self.front_mod *args ; mod_container = mod *args ; mod_container.sub_type = :front     ; return mod_container ; end
    def self.pre_mod   *args ; mod_container = mod *args ; mod_container.sub_type = :pre       ; return mod_container ; end
    def self.post_mod  *args ; mod_container = mod *args ; mod_container.sub_type = :post      ; return mod_container ; end
    def self.adj       *args ; mod_container = mod *args ; mod_container.sub_type = :adjective ; return mod_container ; end

    def self.noun_phrase_helper word_or_hash
      return nil if word_or_hash.nil?
      if word_or_hash.respond_to? :has_key? # testing if it's a Hash, but in a more ducktypingy way than is_a?(Hash)
        np = @@factory.create_noun_phrase word_or_hash[:noun]
        np.set_specifier( (spec = self.noun_phrase_helper(word_or_hash[:specifier]); spec.set_feature(Feature::POSSESSIVE, true) unless spec.nil?; spec) || word_or_hash[:determiner])
        np
      else
        @@factory.create_noun_phrase word_or_hash
      end
    end


    class Container
      attr_accessor :type, :sub_type, :content
      def initialize type, *content
        @type, @content = type, content.flatten
      end
      def is type
        @type == type
      end
    end

  end

end