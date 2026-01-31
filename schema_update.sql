-- Enhanced Case Management Features - Database Schema Update
-- GES Teacher Support Helpline
-- This script adds tables for Case Templates, Follow-ups, and Escalation tracking

USE teacher_query_7;

-- =====================================================
-- Case Templates - Pre-defined responses for common issues
-- =====================================================
CREATE TABLE IF NOT EXISTS case_templates (
  template_id INT AUTO_INCREMENT PRIMARY KEY,
  template_name VARCHAR(150) NOT NULL,
  template_category VARCHAR(50) NOT NULL,  -- e.g., 'Payroll', 'CPD', 'Transfer', 'General', etc.
  template_subject VARCHAR(200) NOT NULL,
  template_body TEXT NOT NULL,
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- =====================================================
-- Follow-ups table for scheduled reminders
-- =====================================================
CREATE TABLE IF NOT EXISTS follow_ups (
  follow_up_id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id BIGINT NOT NULL,
  follow_up_date DATE NOT NULL,
  follow_up_notes TEXT,
  status ENUM('Pending', 'Completed', 'Overdue', 'Cancelled') DEFAULT 'Pending',
  created_by_user_id INT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  completed_at TIMESTAMP NULL,
  completed_by_user_id INT NULL,

  CONSTRAINT fk_followups_ticket FOREIGN KEY (ticket_id) REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_followups_created_by FOREIGN KEY (created_by_user_id) REFERENCES users(user_id),
  CONSTRAINT fk_followups_completed_by FOREIGN KEY (completed_by_user_id) REFERENCES users(user_id)
);

-- =====================================================
-- Add follow_up columns to tickets table (if not exists)
-- =====================================================
ALTER TABLE tickets
ADD COLUMN IF NOT EXISTS next_follow_up_date DATE NULL,
ADD COLUMN IF NOT EXISTS escalation_reason TEXT NULL;

-- =====================================================
-- Indexes for performance
-- =====================================================
CREATE INDEX idx_templates_category ON case_templates(template_category, is_active);
CREATE INDEX idx_followups_date_status ON follow_ups(follow_up_date, status);
CREATE INDEX idx_followups_ticket ON follow_ups(ticket_id);
CREATE INDEX idx_tickets_follow_up ON tickets(next_follow_up_date, status);
CREATE INDEX idx_tickets_escalated ON tickets(status, escalated_at);

-- =====================================================
-- Pre-populated Case Templates based on GES Playbook
-- =====================================================

-- Payroll Templates
INSERT INTO case_templates (template_name, template_category, template_subject, template_body) VALUES
('Salary Delay Acknowledgement', 'Payroll', 'Acknowledgement - Salary Payment Issue',
'Dear Teacher,

Thank you for contacting the GES Teacher Helpline regarding your salary concern.

I have logged your case as a payroll query and escalated it to the Payroll Unit. Your reference number is {case_code}.

The Payroll Unit will respond within 24 working hours. Please keep this reference number for future follow-up.

Is there anything else I can help you with today?

Best regards,
GES Teacher Helpline'),

('Salary Verification Request', 'Payroll', 'Information Required - Salary Query',
'Dear Teacher,

To assist with your salary issue, we need to verify the following:
- Staff ID
- Number of months salary is delayed
- Bank used for salary payments
- Have you recently changed your phone number or bank details?

Please provide these details so we can escalate to the HR Payroll Unit.

Typical resolution time is 7-30 days. We will keep you updated on progress.

Best regards,
GES Teacher Helpline'),

-- CPD/Licensing Templates
('CPD Enquiry Response', 'CPD/Licensing', 'Response - CPD/Licensing Query',
'Dear Teacher,

Thank you for your query about CPD/Licensing.

{response_details}

For further assistance, please visit the Teacher Portal or contact your District Education Office.

Your reference number is {case_code}.

Best regards,
GES Teacher Helpline'),

('Portal Login Issue', 'ICT', 'Response - Portal Access Issue',
'Dear Teacher,

I understand you are having difficulty accessing the Teacher Portal.

Please try the following steps:
1. Clear your browser cache and cookies
2. Try using a different browser (Chrome, Firefox, Edge)
3. Ensure you are using the correct Staff ID
4. If you forgot your password, use the "Forgot Password" link

If the issue persists, please provide your Staff ID and we will escalate to the ICT Unit for password reset.

Your reference number is {case_code}.

Best regards,
GES Teacher Helpline'),

-- Transfer Templates
('Transfer Request Acknowledgement', 'Transfer', 'Acknowledgement - Transfer Request Query',
'Dear Teacher,

Thank you for contacting us regarding your transfer request.

I have logged your concern and will escalate to the HR Directorate. Your reference number is {case_code}.

Please note:
- Transfer requests are processed according to GES policy and available vacancies
- The District HR will be notified of your request
- You will receive an update within 3-5 working days

Best regards,
GES Teacher Helpline'),

-- Welfare/Emergency Templates
('Welfare Case Acknowledgement', 'Welfare', 'Urgent - Welfare Case Logged',
'Dear Teacher,

I am sorry to hear about your situation. I have logged this as a high-priority welfare case.

Your reference number is {case_code}.

The HR Directorate will contact you within 4 working hours. If you are in immediate danger, please contact local emergency services.

We take your safety seriously and will ensure this matter is addressed promptly.

Best regards,
GES Teacher Helpline'),

('Harassment Report Acknowledgement', 'Welfare', 'Confidential - Report Received',
'Dear Teacher,

Thank you for trusting us with this sensitive matter. I have recorded your report as a confidential welfare case.

Your reference number is {case_code}.

This has been escalated immediately to the HR Directorate and Legal Unit as a high-priority case. You will be contacted within 4 working hours.

Please note: All information shared is treated with strict confidentiality.

If you are in immediate danger, please call local emergency services.

Best regards,
GES Teacher Helpline'),

-- General Templates
('Case Resolved SMS', 'General', 'Case Resolved Notification',
'GES Helpline: Your case (Ref: {case_code}) has been resolved. If you need further help, call us on this helpline.'),

('Case Acknowledgement SMS', 'General', 'Case Received Notification',
'GES Helpline: Your request (Ref: {case_code}) has been received and escalated to the appropriate office. You will receive an update within {timeframe}. Thank you.'),

('Follow-up Required', 'General', 'Follow-up Scheduled',
'Dear Teacher,

Thank you for your patience. A follow-up has been scheduled for {follow_up_date} regarding your case (Ref: {case_code}).

We will contact you on that date with an update. If you have any urgent concerns before then, please call the helpline and reference your case number.

Best regards,
GES Teacher Helpline'),

-- Escalation Templates
('Escalation to HR', 'Escalation', 'Case Escalated to HR Directorate',
'Dear HR Directorate,

A case has been escalated from the Teacher Helpline that requires your attention.

Case Reference: {case_code}
Caller: {teacher_name}
Region: {region}
Category: {category}
Priority: {priority}

Issue Summary:
{summary}

Escalation Reason:
{escalation_reason}

Please respond within 24-72 hours depending on priority.

Best regards,
Regional Helpline Team'),

('Escalation to Legal', 'Escalation', 'Case Escalated to Legal Unit',
'Dear Legal Unit,

A case requiring legal attention has been escalated from the Teacher Helpline.

Case Reference: {case_code}
Caller: {teacher_name}
Region: {region}
Category: Discipline/Misconduct/Grievance
Priority: {priority}

Issue Summary:
{summary}

Please treat this matter with urgency and confidentiality.

Best regards,
Regional Helpline Team');

-- =====================================================
-- View for pending follow-ups (today and overdue)
-- =====================================================
CREATE OR REPLACE VIEW pending_follow_ups AS
SELECT
    f.follow_up_id,
    f.ticket_id,
    t.case_code,
    t.teacher_name,
    t.teacher_phone,
    t.status as ticket_status,
    r.region_name,
    c.category_name,
    f.follow_up_date,
    f.follow_up_notes,
    f.status as follow_up_status,
    CASE
        WHEN f.follow_up_date < CURDATE() THEN 'Overdue'
        WHEN f.follow_up_date = CURDATE() THEN 'Due Today'
        ELSE 'Upcoming'
    END as urgency,
    DATEDIFF(CURDATE(), f.follow_up_date) as days_overdue,
    u.full_name as created_by
FROM follow_ups f
JOIN tickets t ON f.ticket_id = t.ticket_id
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN users u ON f.created_by_user_id = u.user_id
WHERE f.status = 'Pending'
ORDER BY f.follow_up_date ASC;

-- =====================================================
-- View for escalated cases (for National PRO Office)
-- =====================================================
CREATE OR REPLACE VIEW escalated_cases_view AS
SELECT
    t.ticket_id,
    t.case_code,
    t.created_at,
    t.escalated_at,
    t.teacher_name,
    t.teacher_phone,
    t.teacher_staff_id,
    t.school_name,
    t.district,
    r.region_name,
    c.category_name,
    t.priority,
    t.status,
    t.summary,
    t.description,
    t.escalation_reason,
    TIMESTAMPDIFF(HOUR, t.escalated_at, NOW()) as hours_since_escalation,
    e.escalation_reason as detailed_reason,
    e.escalation_status,
    u_by.full_name as escalated_by,
    u_to.full_name as escalated_to
FROM tickets t
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN escalations e ON t.ticket_id = e.ticket_id
LEFT JOIN users u_by ON e.escalated_by_user_id = u_by.user_id
LEFT JOIN users u_to ON e.escalated_to_user_id = u_to.user_id
WHERE t.status = 'Escalated'
ORDER BY t.escalated_at DESC;

SELECT 'Schema update complete - Case Templates, Follow-ups, and Escalation tracking added!' as Status;
