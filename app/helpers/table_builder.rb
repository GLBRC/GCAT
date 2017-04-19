module TableBuilder

	# I decided to parse the output file produced by the R library
	# and build an html table rather than using the YUI JS library
	# previously implemented.
	#
	# This method takes the result hash returned by the R bridge routine and
	# builds an array of row arrays
	def output_table result, has_layout
		# if the user submits a layout file, there will be more columns in the output file
		offsets = {:pdf => -2, :page => -1}
		if(has_layout)
			offsets[:pdf] = -7
			offsets[:page] = -6
		end
    puts result[:txtFile]
		rel_path = result[:txtFile][0]
		pdf_path = File.dirname @result[:txtFile][0] #Rails.root.join("public/"+result[:pdfFile])
		path = Rails.root.join("public/"+rel_path)
		raise "File not found" unless File.exists? path
		file = File.open path
		output = []
		file.each_line do |line|
			if(output.empty?)
				# the headers seem to be enclosed in quotations
				# will need to parse a bit differently
				l_ar = line.split "\"\t\""
				l_ar.first.delete! "\""
      	l_ar.last.delete! "\"\n"
      	l_ar.collect! {|l| l.gsub ".", " "}
      	output << l_ar
    	else
    		l_ar = line.split "\t"
      	l_ar.last.delete! "\"\n"
      	break unless l_ar.size > 1
      	output << format_row(l_ar, pdf_path, offsets)
    	end
		end
    file.close
		return output
	end

	# this method: removes quotations from strings, truncates floats to 2 sig. digits,
	# and creates html links to PDF files
	# params: 
	# 	l_ar--An array of the one row of data
	#   path--directory path where the pdf file is located
	#   offsets-- a hash containing the negative array offsets for cells needed to build pdf link
	#
	# returns: A formatted array of one row from the output.txt data
	def format_row l_ar, path, offsets
		result = l_ar.collect! do |entry|
			if(entry.to_f.zero?)
				entry.gsub "\"", ""
			elsif(view_context.number_with_precision(entry.to_f, precision: 2, significant: true).size > 15)
				#if it is too big, use scientific notation
				"%E" % view_context.number_with_precision(entry.to_f, precision: 2, significant: true)
			else
				view_context.number_with_precision(entry.to_f, precision: 2, significant: true)
			end
		end
		result[offsets[:page]] = result[offsets[:page]].to_i  # page number
		# result[-7] == pdf.name
		result[0] = "<a href=\"#{path}/#{result[offsets[:pdf]]}?#page=#{result[offsets[:page]]}\" target='_blank'>#{result[0].to_i}</a>".html_safe  # add pdf link to first row
		return result
	end
end
