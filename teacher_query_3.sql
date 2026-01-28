-- GES Teacher Support Helpline & Query Tracking System
-- Enhanced Database Schema v2 - Aligned with Concept Note
-- Includes comprehensive category system and workflow management

CREATE DATABASE IF NOT EXISTS teacher_query_3;
USE teacher_query_3;

-- =====================================================
-- Reference Tables
-- =====================================================

-- Ghana's 16 regions
CREATE TABLE IF NOT EXISTS regions (
  region_id INT AUTO_INCREMENT PRIMARY KEY,
  region_name VARCHAR(100) NOT NULL UNIQUE,
  region_code VARCHAR(10) NOT NULL UNIQUE,
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- User roles and access levels with enhanced role management
CREATE TABLE IF NOT EXISTS users (
  user_id INT AUTO_INCREMENT PRIMARY KEY,
  full_name VARCHAR(150) NOT NULL,
  email VARCHAR(200) UNIQUE NOT NULL,
  phone VARCHAR(20),
  role ENUM('Regional Agent','Regional Supervisor','National Resolver','National Admin') NOT NULL,
  region_id INT NULL, -- NULL for national roles
  is_active BOOLEAN DEFAULT TRUE,
  password_hash VARCHAR(255) NOT NULL,
  last_login TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  CONSTRAINT fk_users_region FOREIGN KEY (region_id) REFERENCES regions(region_id)
);

-- Enhanced issue categories based on concept note
CREATE TABLE IF NOT EXISTS issue_categories (
  category_id INT AUTO_INCREMENT PRIMARY KEY,
  category_code VARCHAR(20) NOT NULL UNIQUE,
  category_name VARCHAR(120) NOT NULL UNIQUE,
  default_national_role VARCHAR(100) NOT NULL,
  first_response_sla_hours INT DEFAULT 24,
  resolution_sla_hours INT DEFAULT 240, -- 10 business days default
  escalation_trigger_hours INT DEFAULT 72,
  description TEXT,
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Subcategories for detailed classification
CREATE TABLE IF NOT EXISTS issue_subcategories (
  subcategory_id INT AUTO_INCREMENT PRIMARY KEY,
  category_id INT NOT NULL,
  subcategory_code VARCHAR(20) NOT NULL,
  subcategory_name VARCHAR(150) NOT NULL,
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_subcategories_category FOREIGN KEY (category_id) REFERENCES issue_categories(category_id),
  UNIQUE KEY unique_subcat_code (category_id, subcategory_code)
);

-- Communication channels
CREATE TABLE IF NOT EXISTS channels (
  channel_id INT AUTO_INCREMENT PRIMARY KEY,
  channel_name VARCHAR(50) NOT NULL UNIQUE,
  is_active BOOLEAN DEFAULT TRUE
);

-- =====================================================
-- Main Operational Tables
-- =====================================================

-- Enhanced tickets table with comprehensive tracking
CREATE TABLE IF NOT EXISTS tickets (
  ticket_id BIGINT AUTO_INCREMENT PRIMARY KEY,
  case_code VARCHAR(30) UNIQUE NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by_user_id INT NULL,
  region_id INT NOT NULL,
  channel_id INT DEFAULT 1,
  
  -- Teacher/Caller information
  teacher_name VARCHAR(120),
  teacher_phone VARCHAR(40) NOT NULL,
  teacher_staff_id VARCHAR(40),
  school_name VARCHAR(200),
  district VARCHAR(120),
  
  -- Issue classification
  category_id INT NOT NULL,
  subcategory_id INT NULL,
  priority ENUM('Low','Medium','High','Urgent') NOT NULL DEFAULT 'Medium',
  status ENUM('New','In Progress','Waiting on Teacher','Escalated','Resolved','Closed') NOT NULL DEFAULT 'New',
  summary TEXT NOT NULL,
  description TEXT NULL,
  
  -- Workflow timestamps
  first_response_at TIMESTAMP NULL,
  escalated_at TIMESTAMP NULL,
  resolved_at TIMESTAMP NULL,
  closed_at TIMESTAMP NULL,
  
  -- SLA tracking
  first_response_due_at TIMESTAMP NULL,
  resolution_due_at TIMESTAMP NULL,
  
  -- Assignments
  assigned_region_user_id INT NULL,
  assigned_national_user_id INT NULL,
  
  -- Additional fields
  risk_level ENUM('Low','Medium','High') NULL, -- For welfare cases
  is_formal_complaint BOOLEAN DEFAULT FALSE, -- For disciplinary cases
  consent_to_contact BOOLEAN DEFAULT TRUE,
  satisfaction_rating TINYINT NULL,
  satisfaction_feedback TEXT NULL,
  closure_reason VARCHAR(200) NULL,
  
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  
  CONSTRAINT fk_tickets_creator FOREIGN KEY (created_by_user_id) REFERENCES users(user_id),
  CONSTRAINT fk_tickets_region FOREIGN KEY (region_id) REFERENCES regions(region_id),
  CONSTRAINT fk_tickets_channel FOREIGN KEY (channel_id) REFERENCES channels(channel_id),
  CONSTRAINT fk_tickets_category FOREIGN KEY (category_id) REFERENCES issue_categories(category_id),
  CONSTRAINT fk_tickets_subcategory FOREIGN KEY (subcategory_id) REFERENCES issue_subcategories(subcategory_id),
  CONSTRAINT fk_tickets_regional_assignee FOREIGN KEY (assigned_region_user_id) REFERENCES users(user_id),
  CONSTRAINT fk_tickets_national_assignee FOREIGN KEY (assigned_national_user_id) REFERENCES users(user_id),
  CONSTRAINT chk_satisfaction_rating CHECK (satisfaction_rating BETWEEN 1 AND 5)
);

-- Comprehensive ticket actions log
CREATE TABLE IF NOT EXISTS ticket_actions (
  action_id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id BIGINT NOT NULL,
  action_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  action_by_user_id INT NULL,
  action_type ENUM('create','note','status_change','escalation','reassignment','attachment','first_response','resolution','closure') NOT NULL,
  action_text TEXT,
  old_status VARCHAR(50) NULL,
  new_status VARCHAR(50) NULL,
  old_assignee_id INT NULL,
  new_assignee_id INT NULL,
  is_internal BOOLEAN DEFAULT FALSE,
  
  CONSTRAINT fk_actions_ticket FOREIGN KEY (ticket_id) REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_actions_user FOREIGN KEY (action_by_user_id) REFERENCES users(user_id)
);



-- Enhanced escalations tracking
CREATE TABLE IF NOT EXISTS escalations (
  escalation_id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id BIGINT NOT NULL,
  escalated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  escalated_by_user_id INT NOT NULL,
  escalated_to_user_id INT NOT NULL,
  escalation_reason TEXT NOT NULL,
  escalation_method ENUM('Email','WhatsApp','Phone','System') DEFAULT 'System',
  escalation_status ENUM('Pending','In Review','Resolved','Rejected') DEFAULT 'Pending',
  resolved_at TIMESTAMP NULL,
  resolution_notes TEXT NULL,
  
  CONSTRAINT fk_escalations_ticket FOREIGN KEY (ticket_id) REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_escalations_by_user FOREIGN KEY (escalated_by_user_id) REFERENCES users(user_id),
  CONSTRAINT fk_escalations_to_user FOREIGN KEY (escalated_to_user_id) REFERENCES users(user_id)
);

-- File attachments
CREATE TABLE IF NOT EXISTS attachments (
  attachment_id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ticket_id BIGINT NOT NULL,
  filename VARCHAR(255) NOT NULL,
  file_path VARCHAR(500) NOT NULL,
  file_size INT NOT NULL,
  mime_type VARCHAR(100),
  uploaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  uploaded_by_user_id INT NOT NULL,
  
  CONSTRAINT fk_attachments_ticket FOREIGN KEY (ticket_id) REFERENCES tickets(ticket_id) ON DELETE CASCADE,
  CONSTRAINT fk_attachments_user FOREIGN KEY (uploaded_by_user_id) REFERENCES users(user_id)
);

-- =====================================================
-- Indexes for Performance
-- =====================================================

CREATE INDEX idx_tickets_region_status_created ON tickets(region_id, status, created_at);
CREATE INDEX idx_tickets_case_code ON tickets(case_code);
CREATE INDEX idx_tickets_status_created ON tickets(status, created_at);
CREATE INDEX idx_tickets_regional_assignee ON tickets(assigned_region_user_id, status);
CREATE INDEX idx_tickets_national_assignee ON tickets(assigned_national_user_id, status);
CREATE INDEX idx_tickets_teacher_phone ON tickets(teacher_phone);
CREATE INDEX idx_tickets_teacher_staff_id ON tickets(teacher_staff_id);
CREATE INDEX idx_tickets_category_priority ON tickets(category_id, priority);
CREATE INDEX idx_tickets_sla_due ON tickets(first_response_due_at, resolution_due_at, status);

CREATE INDEX idx_actions_ticket_date ON ticket_actions(ticket_id, action_at);
CREATE INDEX idx_actions_user_date ON ticket_actions(action_by_user_id, action_at);
CREATE INDEX idx_actions_type_date ON ticket_actions(action_type, action_at);

CREATE INDEX idx_escalations_ticket ON escalations(ticket_id);
CREATE INDEX idx_escalations_to_user_status ON escalations(escalated_to_user_id, escalation_status);

-- =====================================================
-- Initial Data Population - Based on Concept Note
-- =====================================================

-- Insert Ghana's 16 regions
INSERT IGNORE INTO regions (region_name, region_code) VALUES
('Greater Accra Region', 'GAR'),
('Ashanti Region', 'ASH'),
('Western Region', 'WR'),
('Eastern Region', 'ER'),
('Central Region', 'CR'),
('Northern Region', 'NR'),
('Upper East Region', 'UER'),
('Upper West Region', 'UWR'),
('Volta Region', 'VR'),
('Bono Region', 'BAR'),
('Western North Region', 'WNR'),
('Ahafo Region', 'AHR'),
('Bono East Region', 'BER'),
('Oti Region', 'OR'),
('North East Region', 'NER'),
('Savannah Region', 'SR');

-- Insert communication channels
INSERT IGNORE INTO channels (channel_name) VALUES
('Regional Phone'),
('Toll-free'),
('Walk-in'),
('Telegram'),
('Facebook'),
('Portal'),
('WhatsApp'),
('Email'),
('Other');

-- Insert issue categories from concept note
INSERT IGNORE INTO issue_categories (category_code, category_name, default_national_role, first_response_sla_hours, resolution_sla_hours, escalation_trigger_hours) VALUES
('HR', 'Human Resource Management', 'Director of HR', 24, 240, 72),
('DEPLOY', 'Posting, Deployment and Transfers', 'Director of HR', 24, 240, 72),
('PAY', 'Payroll and Compensation', 'Director of HR', 24, 240, 72),
('LIC', 'Licensing, Registration and Professional Status', 'Director of Schools and Instruction', 24, 240, 72),
('TLI', 'Teaching and Learning / Instructional Issues', 'Director of Schools and Instruction', 48, 240, 72),
('ADMIN', 'School Administration and Leadership', 'Director of Schools and Instruction', 48, 240, 72),
('WEL', 'Welfare, Safety and Wellbeing', 'National PRO', 8, 48, 24),
('DISC', 'Discipline, Misconduct and Grievances', 'Director Legal', 24, 240, 72),
('PART', 'Partnerships, Affiliations and External Placements', 'Director of Partnership and Affiliation', 48, 360, 72),
('SEC', 'Secondary Education Specific Issues', 'Director Secondary Education', 48, 240, 72),
('BASIC', 'Basic Education Specific Issues', 'Director Basic Education', 48, 240, 72),
('ICT', 'ICT Systems and Data Issues', 'National PRO', 24, 120, 48),
('INFO', 'Information Request / Clarification', 'National PRO', 48, 120, 48),
('OTHER', 'Other / Not Classified', 'National PRO', 48, 240, 72);

-- Insert subcategories from concept note
INSERT IGNORE INTO issue_subcategories (category_id, subcategory_code, subcategory_name) VALUES
-- HR subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_PROMO', 'Promotion / upgrading'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_CONFIRM', 'Confirmation / regularisation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_LEAVE', 'Leave (annual, study leave, sick leave)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_RETURN', 'Return to post after leave'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_APPRAISAL', 'Appraisal / supervision process (HR side)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'HR'), 'HR_RECORD', 'Personal record correction (DOB, name, grade, etc.)'),

-- DEPLOY subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_POSTING', 'Posting after training / return from leave'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_TRANSFER', 'Transfer request / complaint'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_REPORTING', 'Reporting to duty challenges'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DEPLOY'), 'DEP_EXCESS', 'Excess/shortage and redistribution concerns'),

-- PAY subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_MISSING', 'Missing salary'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_DELAY', 'Delayed salary'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_ARREARS', 'Arrears'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_DEDUCT', 'Deductions / SSNIT / loan recovery issues'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PAY'), 'PAY_ALLOW', 'Allowances (acting, responsibility, etc.)'),

-- LIC subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_STATUS', 'Licensing status (pending, approved, rejected)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_REG', 'Registration / enrolment delays'),
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_RENEW', 'Renewal'),
((SELECT category_id FROM issue_categories WHERE category_code = 'LIC'), 'LIC_COMPLY', 'Compliance requirements affecting eligibility'),

-- TLI subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_CURR', 'Curriculum implementation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_ASSESS', 'Assessment and grading practices'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_SUBJECT', 'Subject placement / teaching load'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_TIMETABLE', 'Timetabling / contact hours'),
((SELECT category_id FROM issue_categories WHERE category_code = 'TLI'), 'TLI_TLM', 'Teaching and learning materials'),

-- ADMIN subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_HEAD', 'Headteacher leadership concern'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_SCHOOLMGT', 'School management dispute/process'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_DIRECTIVE', 'Implementation of directives at school level'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ADMIN'), 'ADM_WORKLOAD', 'Duty allocation and workload disputes'),

-- WEL subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_SAFETY', 'Safety and security threat'),
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_HARASS', 'Harassment / intimidation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_HEALTH', 'Health / wellbeing support need'),
((SELECT category_id FROM issue_categories WHERE category_code = 'WEL'), 'WEL_EMERG', 'Emergency welfare case'),

-- DISC subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_ALLEG', 'Misconduct allegation (teacher)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_PROC', 'Disciplinary process / due process concern'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_GRIEV', 'Formal grievance against supervisor/institution'),
((SELECT category_id FROM issue_categories WHERE category_code = 'DISC'), 'DISC_SANCT', 'Sanctions / appeals'),

-- PART subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_AFFIL', 'School affiliation matters'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_FAITH', 'Faith-based / mission school arrangements'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_NGO', 'Partner-supported school issues affecting teachers'),
((SELECT category_id FROM issue_categories WHERE category_code = 'PART'), 'PART_MOU', 'MoU interpretation affecting operations'),

-- SEC subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'SEC'), 'SEC_SHS', 'SHS/SHTS policy issue'),
((SELECT category_id FROM issue_categories WHERE category_code = 'SEC'), 'SEC_BOARD', 'Boarding duty / boarding-related concerns'),
((SELECT category_id FROM issue_categories WHERE category_code = 'SEC'), 'SEC_SUBCOMB', 'Subject combinations / placement'),

-- BASIC subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'BASIC'), 'BAS_DEPLOY', 'Basic school deployment / multi-grade challenges'),
((SELECT category_id FROM issue_categories WHERE category_code = 'BASIC'), 'BAS_RURAL', 'Rural posting hardship concerns (basic)'),
((SELECT category_id FROM issue_categories WHERE category_code = 'BASIC'), 'BAS_HEAD', 'Basic school leadership/management issue'),

-- ICT subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_PORTAL', 'Portal access issues'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_DATAERR', 'Data error affecting records'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_RESET', 'Password reset / account recovery'),
((SELECT category_id FROM issue_categories WHERE category_code = 'ICT'), 'ICT_DEVICE', 'Device/app/platform issue linked to work'),

-- INFO subcategories
((SELECT category_id FROM issue_categories WHERE category_code = 'INFO'), 'INFO_POLICY', 'Policy clarification'),
((SELECT category_id FROM issue_categories WHERE category_code = 'INFO'), 'INFO_CIRC', 'Circular interpretation'),
((SELECT category_id FROM issue_categories WHERE category_code = 'INFO'), 'INFO_STATUS', 'Status follow-up on existing case'),

-- OTHER subcategory
((SELECT category_id FROM issue_categories WHERE category_code = 'OTHER'), 'OTHER_GEN', 'Not classified (review weekly)');

-- Insert sample users
INSERT IGNORE INTO users (full_name, email, role, region_id, password_hash) VALUES
('John Doe', 'john.doe@ges.gov.gh', 'Regional Agent', 1, '$2y$10$92IXUNpkjO0rOQ5byMi.Ye4oKoEa3Ro9llC/.og/at2.uheWG/igi'),
('Jane Smith', 'jane.smith@ges.gov.gh', 'Regional Supervisor', 1, '$2y$10$92IXUNpkjO0rOQ5byMi.Ye4oKoEa3Ro9llC/.og/at2.uheWG/igi'),
('Robert Johnson', 'robert.johnson@ges.gov.gh', 'National Resolver', NULL, '$2y$10$92IXUNpkjO0rOQ5byMi.Ye4oKoEa3Ro9llC/.og/at2.uheWG/igi'),
('Mary Administrator', 'admin@ges.gov.gh', 'National Admin', NULL, 'game1990');

-- =====================================================
-- Triggers for Auto-Updates
-- =====================================================

-- Auto-generate case codes and set SLA dates
DELIMITER //
CREATE TRIGGER set_ticket_defaults 
BEFORE INSERT ON tickets
FOR EACH ROW
BEGIN
    DECLARE sla_first_response INT;
    DECLARE sla_resolution INT;
    
    -- Generate case code if not provided
    IF NEW.case_code IS NULL OR NEW.case_code = '' THEN
        SET NEW.case_code = CONCAT('GES-', YEAR(NOW()), '-', LPAD((SELECT COALESCE(MAX(ticket_id), 0) + 1 FROM tickets), 6, '0'));
    END IF;
    
    -- Get SLA hours for category
    SELECT first_response_sla_hours, resolution_sla_hours 
    INTO sla_first_response, sla_resolution
    FROM issue_categories 
    WHERE category_id = NEW.category_id;
    
    -- Set SLA due dates
    IF NEW.first_response_due_at IS NULL THEN
        SET NEW.first_response_due_at = DATE_ADD(NEW.created_at, INTERVAL sla_first_response HOUR);
    END IF;
    
    IF NEW.resolution_due_at IS NULL THEN
        SET NEW.resolution_due_at = DATE_ADD(NEW.created_at, INTERVAL sla_resolution HOUR);
    END IF;
END//
DELIMITER ;

-- Auto-log ticket creation
DELIMITER //
CREATE TRIGGER log_ticket_creation 
AFTER INSERT ON tickets
FOR EACH ROW
BEGIN
    INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, new_status)
    VALUES (NEW.ticket_id, NEW.created_by_user_id, 'create', 
            CONCAT('Case logged via ', (SELECT channel_name FROM channels WHERE channel_id = NEW.channel_id)), 
            NEW.status);
END//
DELIMITER ;

-- Auto-log status changes
DELIMITER //
CREATE TRIGGER log_status_changes 
AFTER UPDATE ON tickets
FOR EACH ROW
BEGIN
    IF OLD.status != NEW.status THEN
        INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, old_status, new_status)
        VALUES (NEW.ticket_id, COALESCE(NEW.assigned_region_user_id, NEW.assigned_national_user_id, NEW.created_by_user_id), 
                'status_change', 
                CONCAT('Status changed from ', OLD.status, ' to ', NEW.status),
                OLD.status, NEW.status);
    END IF;
END//
DELIMITER ;

-- =====================================================
-- Views for Dashboard and Reporting
-- =====================================================

-- SLA monitoring view
CREATE OR REPLACE VIEW sla_monitoring AS
SELECT 
    t.ticket_id,
    t.case_code,
    t.created_at,
    r.region_name,
    c.category_name,
    t.priority,
    t.status,
    t.first_response_due_at,
    t.resolution_due_at,
    CASE 
        WHEN t.status IN ('New', 'In Progress') AND t.first_response_at IS NULL AND t.first_response_due_at < NOW() THEN 'First Response Overdue'
        WHEN t.status IN ('New', 'In Progress') AND t.first_response_at IS NULL AND t.first_response_due_at < DATE_ADD(NOW(), INTERVAL 2 HOUR) THEN 'First Response Due Soon'
        WHEN t.status NOT IN ('Resolved', 'Closed') AND t.resolution_due_at < NOW() THEN 'Resolution Overdue'
        WHEN t.status NOT IN ('Resolved', 'Closed') AND t.resolution_due_at < DATE_ADD(NOW(), INTERVAL 4 HOUR) THEN 'Resolution Due Soon'
        ELSE 'On Track'
    END as sla_status,
    TIMESTAMPDIFF(HOUR, t.created_at, NOW()) as hours_open,
    u1.full_name as regional_assignee,
    u2.full_name as national_assignee
FROM tickets t
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN users u1 ON t.assigned_region_user_id = u1.user_id
LEFT JOIN users u2 ON t.assigned_national_user_id = u2.user_id
WHERE t.status NOT IN ('Closed');

-- Dashboard statistics view
CREATE OR REPLACE VIEW dashboard_stats AS
SELECT 
    r.region_name,
    DATE(t.created_at) as case_date,
    t.status,
    t.priority,
    c.category_name,
    c.category_code,
    COUNT(*) as case_count,
    AVG(TIMESTAMPDIFF(HOUR, t.created_at, COALESCE(t.resolved_at, NOW()))) as avg_hours_to_resolve,
    COUNT(CASE WHEN t.resolved_at IS NOT NULL THEN 1 END) as resolved_count,
    COUNT(CASE WHEN t.status = 'Escalated' THEN 1 END) as escalated_count,
    COUNT(CASE WHEN sla.sla_status LIKE '%Overdue%' THEN 1 END) as overdue_count
FROM tickets t
JOIN regions r ON t.region_id = r.region_id
JOIN issue_categories c ON t.category_id = c.category_id
LEFT JOIN sla_monitoring sla ON t.ticket_id = sla.ticket_id
GROUP BY r.region_name, DATE(t.created_at), t.status, t.priority, c.category_name, c.category_code;

-- =====================================================
-- Stored Procedures
-- =====================================================

-- Procedure to escalate ticket
DELIMITER //
CREATE PROCEDURE EscalateTicket(
    IN p_ticket_id BIGINT,
    IN p_escalated_by_user_id INT,
    IN p_escalated_to_user_id INT,
    IN p_reason TEXT
)
BEGIN
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
        ROLLBACK;
        RESIGNAL;
    END;
    
    START TRANSACTION;
    
    -- Update ticket
    UPDATE tickets 
    SET status = 'Escalated', 
        assigned_national_user_id = p_escalated_to_user_id,
        escalated_at = NOW()
    WHERE ticket_id = p_ticket_id;
    
    -- Create escalation record
    INSERT INTO escalations (ticket_id, escalated_by_user_id, escalated_to_user_id, escalation_reason)
    VALUES (p_ticket_id, p_escalated_by_user_id, p_escalated_to_user_id, p_reason);
    
    -- Log the action
    INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, new_status)
    VALUES (p_ticket_id, p_escalated_by_user_id, 'escalation', p_reason, 'Escalated');
    
    COMMIT;
END//
DELIMITER ;

-- Procedure to resolve ticket
DELIMITER //
CREATE PROCEDURE ResolveTicket(
    IN p_ticket_id BIGINT,
    IN p_resolved_by_user_id INT,
    IN p_resolution_notes TEXT
)
BEGIN
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
        ROLLBACK;
        RESIGNAL;
    END;
    
    START TRANSACTION;
    
    -- Update ticket
    UPDATE tickets 
    SET status = 'Resolved', 
        resolved_at = NOW()
    WHERE ticket_id = p_ticket_id;
    
    -- Log the action
    INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, new_status)
    VALUES (p_ticket_id, p_resolved_by_user_id, 'resolution', p_resolution_notes, 'Resolved');
    
    COMMIT;
END//
DELIMITER ;

SELECT 'Enhanced GES Teacher Support Helpline Database Setup Complete!' as Status,
       'Database now includes comprehensive categories, subcategories, SLA tracking, and workflow management.' as Details;


SELECT * FROM sla_monitoring WHERE sla_status <> 'On Track' ORDER BY created_at DESC;
