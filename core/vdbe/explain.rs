use super::{Insn, InsnReference, OwnedValue, Program};
use std::rc::Rc;

pub fn insn_to_str(
    program: &Program,
    addr: InsnReference,
    insn: &Insn,
    indent: String,
    manual_comment: Option<&'static str>,
) -> String {
    let opcode: &str = insn.get_name();

    let (p1, p2, p3, p4, p5, comment): (i32, i32, i32, OwnedValue, u16, String) = match insn {
        Insn::Init { target_pc } => (
            0,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("Start at {}", target_pc),
        ),
        Insn::Add { lhs, rhs, dest } => (
            *lhs as i32,
            *rhs as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]+r[{}]", dest, lhs, rhs),
        ),
        Insn::Subtract { lhs, rhs, dest } => (
            *lhs as i32,
            *rhs as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]-r[{}]", dest, lhs, rhs),
        ),
        Insn::Multiply { lhs, rhs, dest } => (
            *lhs as i32,
            *rhs as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]*r[{}]", dest, lhs, rhs),
        ),
        Insn::Divide { lhs, rhs, dest } => (
            *lhs as i32,
            *rhs as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]/r[{}]", dest, lhs, rhs),
        ),
        Insn::BitAnd { lhs, rhs, dest } => (
            *lhs as i32,
            *rhs as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]&r[{}]", dest, lhs, rhs),
        ),
        Insn::BitOr { lhs, rhs, dest } => (
            *lhs as i32,
            *rhs as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]|r[{}]", dest, lhs, rhs),
        ),
        Insn::BitNot { reg, dest } => (
            *reg as i32,
            *dest as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=~r[{}]", dest, reg),
        ),
        Insn::Null { dest, dest_end } => (
            0,
            *dest as i32,
            dest_end.map_or(0, |end| end as i32),
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            dest_end.map_or(format!("r[{}]=NULL", dest), |end| {
                format!("r[{}..{}]=NULL", dest, end)
            }),
        ),
        Insn::NullRow { cursor_id } => (
            *cursor_id as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("Set cursor {} to a (pseudo) NULL row", cursor_id),
        ),
        Insn::NotNull { reg, target_pc } => (
            *reg as i32,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]!=NULL -> goto {}", reg, target_pc),
        ),
        Insn::Compare {
            start_reg_a,
            start_reg_b,
            count,
        } => (
            *start_reg_a as i32,
            *start_reg_b as i32,
            *count as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "r[{}..{}]==r[{}..{}]",
                start_reg_a,
                start_reg_a + (count - 1),
                start_reg_b,
                start_reg_b + (count - 1)
            ),
        ),
        Insn::Jump {
            target_pc_lt,
            target_pc_eq,
            target_pc_gt,
        } => (
            *target_pc_lt as i32,
            *target_pc_eq as i32,
            *target_pc_gt as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Move {
            source_reg,
            dest_reg,
            count,
        } => (
            *source_reg as i32,
            *dest_reg as i32,
            *count as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "r[{}..{}]=r[{}..{}]",
                dest_reg,
                dest_reg + (count - 1),
                source_reg,
                source_reg + (count - 1)
            ),
        ),
        Insn::IfPos {
            reg,
            target_pc,
            decrement_by,
        } => (
            *reg as i32,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "r[{}]>0 -> r[{}]-={}, goto {}",
                reg, reg, decrement_by, target_pc
            ),
        ),
        Insn::Eq {
            lhs,
            rhs,
            target_pc,
        } => (
            *lhs as i32,
            *rhs as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}]==r[{}] goto {}", lhs, rhs, target_pc),
        ),
        Insn::Ne {
            lhs,
            rhs,
            target_pc,
        } => (
            *lhs as i32,
            *rhs as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}]!=r[{}] goto {}", lhs, rhs, target_pc),
        ),
        Insn::Lt {
            lhs,
            rhs,
            target_pc,
        } => (
            *lhs as i32,
            *rhs as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}]<r[{}] goto {}", lhs, rhs, target_pc),
        ),
        Insn::Le {
            lhs,
            rhs,
            target_pc,
        } => (
            *lhs as i32,
            *rhs as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}]<=r[{}] goto {}", lhs, rhs, target_pc),
        ),
        Insn::Gt {
            lhs,
            rhs,
            target_pc,
        } => (
            *lhs as i32,
            *rhs as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}]>r[{}] goto {}", lhs, rhs, target_pc),
        ),
        Insn::Ge {
            lhs,
            rhs,
            target_pc,
        } => (
            *lhs as i32,
            *rhs as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}]>=r[{}] goto {}", lhs, rhs, target_pc),
        ),
        Insn::If {
            reg,
            target_pc,
            null_reg,
        } => (
            *reg as i32,
            *target_pc as i32,
            *null_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if r[{}] goto {}", reg, target_pc),
        ),
        Insn::IfNot {
            reg,
            target_pc,
            null_reg,
        } => (
            *reg as i32,
            *target_pc as i32,
            *null_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if !r[{}] goto {}", reg, target_pc),
        ),
        Insn::OpenReadAsync {
            cursor_id,
            root_page,
        } => (
            *cursor_id as i32,
            *root_page as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "table={}, root={}",
                program.cursor_ref[*cursor_id]
                    .0
                    .as_ref()
                    .unwrap_or(&format!("cursor {}", cursor_id)),
                root_page
            ),
        ),
        Insn::OpenReadAwait => (
            0,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::OpenPseudo {
            cursor_id,
            content_reg,
            num_fields,
        } => (
            *cursor_id as i32,
            *content_reg as i32,
            *num_fields as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("{} columns in r[{}]", num_fields, content_reg),
        ),
        Insn::RewindAsync { cursor_id } => (
            *cursor_id as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::RewindAwait {
            cursor_id,
            pc_if_empty,
        } => (
            *cursor_id as i32,
            *pc_if_empty as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "Rewind table {}",
                program.cursor_ref[*cursor_id]
                    .0
                    .as_ref()
                    .unwrap_or(&format!("cursor {}", cursor_id))
            ),
        ),
        Insn::Column {
            cursor_id,
            column,
            dest,
        } => {
            let (table_identifier, table) = &program.cursor_ref[*cursor_id];
            (
                *cursor_id as i32,
                *column as i32,
                *dest as i32,
                OwnedValue::Text(Rc::new("".to_string())),
                0,
                format!(
                    "r[{}]={}.{}",
                    dest,
                    table_identifier
                        .as_ref()
                        .unwrap_or(&format!("cursor {}", cursor_id)),
                    table
                        .as_ref()
                        .and_then(|x| x.column_index_to_name(*column))
                        .unwrap_or(format!("column {}", *column).as_str())
                ),
            )
        }
        Insn::MakeRecord {
            start_reg,
            count,
            dest_reg,
        } => (
            *start_reg as i32,
            *count as i32,
            *dest_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "r[{}]=mkrec(r[{}..{}])",
                dest_reg,
                start_reg,
                start_reg + count - 1,
            ),
        ),
        Insn::ResultRow { start_reg, count } => (
            *start_reg as i32,
            *count as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            if *count == 1 {
                format!("output=r[{}]", start_reg)
            } else {
                format!("output=r[{}..{}]", start_reg, start_reg + count - 1)
            },
        ),
        Insn::NextAsync { cursor_id } => (
            *cursor_id as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::NextAwait {
            cursor_id,
            pc_if_next,
        } => (
            *cursor_id as i32,
            *pc_if_next as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Halt {
            err_code,
            description: _,
        } => (
            *err_code as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Transaction { write } => (
            0,
            *write as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Goto { target_pc } => (
            0,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Gosub {
            target_pc,
            return_reg,
        } => (
            *return_reg as i32,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Return { return_reg } => (
            *return_reg as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Integer { value, dest } => (
            *value as i32,
            *dest as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]={}", dest, value),
        ),
        Insn::Real { value, dest } => (
            0,
            *dest as i32,
            0,
            OwnedValue::Float(*value),
            0,
            format!("r[{}]={}", dest, value),
        ),
        Insn::RealAffinity { register } => (
            *register as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::String8 { value, dest } => (
            0,
            *dest as i32,
            0,
            OwnedValue::Text(Rc::new(value.clone())),
            0,
            format!("r[{}]='{}'", dest, value),
        ),
        Insn::Blob { value, dest } => (
            0,
            *dest as i32,
            0,
            OwnedValue::Blob(Rc::new(value.clone())),
            0,
            format!(
                "r[{}]={} (len={})",
                dest,
                String::from_utf8_lossy(value),
                value.len()
            ),
        ),
        Insn::RowId { cursor_id, dest } => (
            *cursor_id as i32,
            *dest as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "r[{}]={}.rowid",
                dest,
                &program.cursor_ref[*cursor_id]
                    .0
                    .as_ref()
                    .unwrap_or(&format!("cursor {}", cursor_id))
            ),
        ),
        Insn::SeekRowid {
            cursor_id,
            src_reg,
            target_pc,
        } => (
            *cursor_id as i32,
            *src_reg as i32,
            *target_pc as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!(
                "if (r[{}]!={}.rowid) goto {}",
                src_reg,
                &program.cursor_ref[*cursor_id]
                    .0
                    .as_ref()
                    .unwrap_or(&format!("cursor {}", cursor_id)),
                target_pc
            ),
        ),
        Insn::DeferredSeek {
            index_cursor_id,
            table_cursor_id,
        } => (
            *index_cursor_id as i32,
            *table_cursor_id as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::SeekGT {
            is_index: _,
            cursor_id,
            start_reg,
            num_regs: _,
            target_pc,
        } => (
            *cursor_id as i32,
            *target_pc as i32,
            *start_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::SeekGE {
            is_index: _,
            cursor_id,
            start_reg,
            num_regs: _,
            target_pc,
        } => (
            *cursor_id as i32,
            *target_pc as i32,
            *start_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::IdxGT {
            cursor_id,
            start_reg,
            num_regs: _,
            target_pc,
        } => (
            *cursor_id as i32,
            *target_pc as i32,
            *start_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::IdxGE {
            cursor_id,
            start_reg,
            num_regs: _,
            target_pc,
        } => (
            *cursor_id as i32,
            *target_pc as i32,
            *start_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::DecrJumpZero { reg, target_pc } => (
            *reg as i32,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if (--r[{}]==0) goto {}", reg, target_pc),
        ),
        Insn::AggStep {
            func,
            acc_reg,
            delimiter: _,
            col,
        } => (
            0,
            *col as i32,
            *acc_reg as i32,
            OwnedValue::Text(Rc::new(func.to_string().into())),
            0,
            format!("accum=r[{}] step(r[{}])", *acc_reg, *col),
        ),
        Insn::AggFinal { register, func } => (
            0,
            *register as i32,
            0,
            OwnedValue::Text(Rc::new(func.to_string().into())),
            0,
            format!("accum=r[{}]", *register),
        ),
        Insn::SorterOpen {
            cursor_id,
            columns,
            order,
        } => {
            let _p4 = String::new();
            let to_print: Vec<String> = order
                .values
                .iter()
                .map(|v| match v {
                    OwnedValue::Integer(i) => {
                        if *i == 0 {
                            "B".to_string()
                        } else {
                            "-B".to_string()
                        }
                    }
                    _ => unreachable!(),
                })
                .collect();
            (
                *cursor_id as i32,
                *columns as i32,
                0,
                OwnedValue::Text(Rc::new(format!(
                    "k({},{})",
                    order.values.len(),
                    to_print.join(",")
                ))),
                0,
                format!("cursor={}", cursor_id),
            )
        }
        Insn::SorterData {
            cursor_id,
            dest_reg,
            pseudo_cursor,
        } => (
            *cursor_id as i32,
            *dest_reg as i32,
            *pseudo_cursor as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=data", dest_reg),
        ),
        Insn::SorterInsert {
            cursor_id,
            record_reg,
        } => (
            *cursor_id as i32,
            *record_reg as i32,
            0,
            OwnedValue::Integer(0),
            0,
            format!("key=r[{}]", record_reg),
        ),
        Insn::SorterSort {
            cursor_id,
            pc_if_empty,
        } => (
            *cursor_id as i32,
            *pc_if_empty as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::SorterNext {
            cursor_id,
            pc_if_next,
        } => (
            *cursor_id as i32,
            *pc_if_next as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Function {
            constant_mask,
            start_reg,
            dest,
            func,
        } => (
            *constant_mask,
            *start_reg as i32,
            *dest as i32,
            OwnedValue::Text(Rc::new(func.func.to_string())),
            0,
            if func.arg_count == 0 {
                format!("r[{}]=func()", dest)
            } else if *start_reg == *start_reg + func.arg_count - 1 {
                format!("r[{}]=func(r[{}])", dest, start_reg)
            } else {
                format!(
                    "r[{}]=func(r[{}..{}])",
                    dest,
                    start_reg,
                    start_reg + func.arg_count - 1
                )
            },
        ),
        Insn::InitCoroutine {
            yield_reg,
            jump_on_definition,
            start_offset,
        } => (
            *yield_reg as i32,
            *jump_on_definition as i32,
            *start_offset as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::EndCoroutine { yield_reg } => (
            *yield_reg as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Yield {
            yield_reg,
            end_offset,
        } => (
            *yield_reg as i32,
            *end_offset as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::InsertAsync {
            cursor,
            key_reg,
            record_reg,
            flag,
        } => (
            *cursor as i32,
            *record_reg as i32,
            *key_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            *flag as u16,
            "".to_string(),
        ),
        Insn::InsertAwait { cursor_id } => (
            *cursor_id as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::NewRowid {
            cursor,
            rowid_reg,
            prev_largest_reg,
        } => (
            *cursor as i32,
            *rowid_reg as i32,
            *prev_largest_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::MustBeInt { reg } => (
            *reg as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::SoftNull { reg } => (
            *reg as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::NotExists {
            cursor,
            rowid_reg,
            target_pc,
        } => (
            *cursor as i32,
            *target_pc as i32,
            *rowid_reg as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::OpenWriteAsync {
            cursor_id,
            root_page,
        } => (
            *cursor_id as i32,
            *root_page as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::OpenWriteAwait {} => (
            0,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::Copy {
            src_reg,
            dst_reg,
            amount,
        } => (
            *src_reg as i32,
            *dst_reg as i32,
            *amount as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=r[{}]", dst_reg, src_reg),
        ),
        Insn::CreateBtree { db, root, flags } => (
            *db as i32,
            *root as i32,
            *flags as i32,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("r[{}]=root iDb={} flags={}", root, db, flags),
        ),
        Insn::Close { cursor_id } => (
            *cursor_id as i32,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::LastAsync { .. } => (
            0,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::IsNull { src, target_pc } => (
            *src as i32,
            *target_pc as i32,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            format!("if (r[{}]==NULL) goto {}", src, target_pc),
        ),
        Insn::ParseSchema { db, where_clause } => (
            *db as i32,
            0,
            0,
            OwnedValue::Text(Rc::new(where_clause.clone())),
            0,
            where_clause.clone(),
        ),
        Insn::LastAwait { .. } => (
            0,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::PrevAsync { .. } => (
            0,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
        Insn::PrevAwait { .. } => (
            0,
            0,
            0,
            OwnedValue::Text(Rc::new("".to_string())),
            0,
            "".to_string(),
        ),
    };
    format!(
        "{:<4}  {:<17}  {:<4}  {:<4}  {:<4}  {:<13}  {:<2}  {}",
        addr,
        &(indent + opcode),
        p1,
        p2,
        p3,
        p4.to_string(),
        p5,
        manual_comment.map_or(comment.to_string(), |mc| format!("{}; {}", comment, mc))
    )
}
