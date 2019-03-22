class HelpdeskMailer < ActionMailer::Base
  default from: "gcat.help@glbrc.wisc.edu"
  def reportError(info)
    @name = info[:name]
    @email = info[:email]
    @zip_path = info[:zip_path]
    attachments['debug_info.zip'] = File.read(@zip_path)
    mail(to: "gcat.help@glbrc.wisc.edu", subject: "GCAT Error Report")
  end
end
