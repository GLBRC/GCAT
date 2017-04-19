class HelpdeskMailer < ActionMailer::Base
  default from: "gcat.help@glbrc.wisc.edu"
  def reportError(info)
    @name = info[:name]
    @email = info[:email]
    @zip_path = info[:zip_path]
    attachments['debug_info.zip'] = File.read(@zip_path)
    mail(to: "gcat.help@glbrc.wisc.edu", subject: "GCAT Error Report")
  end

  def sendThankyouLetter(info)
    @name = info[:name]
    @email = info[:email]
    return unless @email.include? '@'
    content = <<EOF
Hi #{@name}:
    Thank you for contacting GCAT development team. We shall investigate and get back to you as soon as we can.

Best,
GCAT development team
EOF
    mail(to: @email, subject: "Thank you for contacing GCAT") do |format|
      format.text {render text: content} 
    end
  end
end
